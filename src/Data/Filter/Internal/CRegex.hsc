{-# LANGUAGE CPP, ForeignFunctionInterface, CApiFFI, EmptyDataDecls #-}

module Data.Filter.Internal.CRegex
( RegexOpt
, optNone
, optIgnoreCase
, optExtend
, optMultiLine
, optSingleLine
, optFindLongest
, optFindNotEmpty

, Regex
, compile
, test
, match
, Match
, MatchCapture

) where

import Prelude hiding (length)

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString (ByteString, useAsCStringLen, packCString, packCStringLen)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import Data.Bifunctor (bimap, first)
import Control.Monad (join, foldM)
import Data.HashMap.Internal.Strict (HashMap)
import qualified Data.HashMap.Internal.Strict as Map

#include "oniguruma.h"

-- Oniguruma regex compile options
newtype RegexOpt = RegexOpt CInt

#{enum RegexOpt, RegexOpt
  , optNone          = ONIG_OPTION_NONE
  , optCaptureGroup  = ONIG_OPTION_CAPTURE_GROUP
  , optIgnoreCase    = ONIG_OPTION_IGNORECASE
  , optExtend        = ONIG_OPTION_EXTEND
  , optMultiLine     = ONIG_OPTION_MULTILINE
  , optSingleLine    = ONIG_OPTION_SINGLELINE
  , optFindLongest   = ONIG_OPTION_FIND_LONGEST
  , optFindNotEmpty  = ONIG_OPTION_FIND_NOT_EMPTY
  }

instance Semigroup RegexOpt where
  RegexOpt opt1 <> RegexOpt opt2 = RegexOpt $ opt1 .|. opt2

instance Monoid RegexOpt where
  mempty = optNone

-- Opaque Oniguruma regex structure
data Onig

-- Structure used by Oniguruma to marshal errors
data OnigErrorInfo

-- Opaque data type we use to store compiled regex information
data Regex = Regex
  { reg_onig      :: ForeignPtr Onig
  , reg_errorInfo :: ForeignPtr OnigErrorInfo
  } deriving (Eq, Show)

-- ((offset, length, stringMatched), captureName)
type MatchCapture = (Maybe (Int, Int, ByteString), Maybe ByteString)

-- (offset, length, stringMatched, captures)
type Match = (Int, Int, ByteString, [MatchCapture])

-- Type of function needed for capture name iteration in onig_foreach_name
type NameIter a = CString -> Ptr CChar -> CInt -> Ptr CInt -> Ptr Onig -> Ptr a -> CInt

-- We use UTF-8
data OnigEncoding

-- We use PerlNG
data OnigSyntax

-- A region is a list of ranges where a match was found
-- First region is the whole match, the rest are the different capture groups
data OnigRegion

peekRegion :: Ptr OnigRegion -> IO [(Int, Int)]
peekRegion reg = do
  regCount  <- (#{peek OnigRegion, num_regs} reg :: IO CInt)
  begs      <- (#{peek OnigRegion, beg} reg :: IO (Ptr CInt))
  ends      <- (#{peek OnigRegion, end} reg :: IO (Ptr CInt))
  mapM (\i -> join bimap fromIntegral <$> ((,) <$> peek (begs `plusIntPtr` i) <*> peek (ends `plusIntPtr` i))) $ take (fromIntegral regCount) [0..]

onigErrorInfoSize :: Int
onigErrorInfoSize = #{size OnigErrorInfo}

onigStatusNormal :: CInt
onigStatusNormal = #{const ONIG_NORMAL}

onigStatusMismatch :: CInt
onigStatusMismatch = #{const ONIG_MISMATCH}

onigMaxErrorMessageLen :: Int
onigMaxErrorMessageLen = #{const ONIG_MAX_ERROR_MESSAGE_LEN}

-- Translate status code to a human readable error message
onigError :: CInt -> Ptr OnigErrorInfo -> IO (Either Text b)
onigError status errInfo =
  allocaBytes onigMaxErrorMessageLen $ \errBuff -> do
    c_onig_error_code_to_str errBuff status errInfo
    errMsg <- packCString errBuff
    return $ Left $ decodeUtf8With lenientDecode errMsg

-- Compile regex pattern
compile :: ByteString -> RegexOpt -> Either Text Regex
compile str opts = unsafePerformIO $
  useAsCStringLen str $ \(pattern, patLen) -> do
      errInfoPtr <- mallocForeignPtrBytes onigErrorInfoSize
      withForeignPtr errInfoPtr $ \errInfo -> do
        alloca $ \regPtr -> do
          status <- c_onig_new regPtr pattern (pattern `plusCharPtr` patLen) (opts <> optCaptureGroup) c_onig_encoding_utf8 c_onig_syntax_perl_ng errInfo
          if status == onigStatusNormal
          then do
            reg <- newForeignPtr c_onig_free =<< peek regPtr
            return $ Right Regex
              { reg_onig      = reg
              , reg_errorInfo = errInfoPtr
              }
          else
            onigError status errInfo -- error

-- Test whether regex matches any part of the string
test :: Regex -> ByteString -> Either Text Bool
test Regex {..} str = unsafePerformIO $
  withForeignPtr reg_onig $ \reg -> do
    useAsCStringLen str $ \(cstr, strLen) -> do
      let start = cstr
      let end   = start `plusCharPtr` strLen
      status <- c_onig_search reg cstr end start end nullPtr mempty
      if status >= 0 then return $ Right True
      else if status == onigStatusMismatch then return $ Right False
      else withForeignPtr reg_errorInfo $ onigError status

match :: Regex -> Bool -> ByteString -> Either Text [Match]
match Regex {..} global str = unsafePerformIO $
  withForeignPtr reg_onig $ \reg -> do
    useAsCStringLen str $ \(s, strLen) -> do
      -- Allocs
      region    <- c_onig_region_new
      nameIter  <- nameIterToFunPtr captureNameIter

      let start = s
      let end   = start `plusCharPtr` strLen
      ret       <- run reg s region nameIter end start

      -- Deallocs
      freeHaskellFunPtr nameIter
      c_onig_region_free region 1

      return $ sequence ret
      where
        run reg s region nameIter end start = do
          -- We don't support execution options
          status <- c_onig_search reg s end start end region mempty
          if status >= 0 then do
            -- First region corresponds to whole string match, the rest are the different captures
            regs <- peekRegion region
            let (beg0, end0) = head regs
            if beg0 == end0
            then do
              -- Empty match
              offset        <- utf8StringLen beg0 s
              let length    = 0
              let string    = ""
              let captures  = []
              let ret       = (offset, length, string, captures)
              (Right ret:) <$> reRun reg s region nameIter end (start `plusCharPtr` 1)
            else do
              let realLen   = end0 - beg0
              let matchStr  = s `plusCharPtr` beg0
              -- Map of capture group index to it's name (if exists)
              captureNameMap  <- captureNames reg nameIter
              offset          <- utf8StringLen beg0 s
              length          <- utf8StringLen realLen matchStr
              string          <- packCStringLen (matchStr, realLen)
              captures        <- mapM (buildCapture s . first (`Map.lookup` captureNameMap)) (zip [0..] $ tail regs)
              let ret         = (offset, length, string, captures)
              -- Before continuing, we free the current search region
              c_onig_region_free region 0
              (Right ret :) <$> reRun reg s region nameIter end (s `plusCharPtr` end0)
          else if status == onigStatusMismatch then
            return []
          else withForeignPtr reg_errorInfo (fmap (:[]) . onigError status)

        reRun reg s region nameIter end start
          | global && start /= end  = run reg s region nameIter end start
          | otherwise               = return []

----------------------------------- Match Helper Functions -----------------------------------------

-- How many bytes the current codepoint takes based on first byte
utf8CodepointLen :: CChar -> Int
utf8CodepointLen c
  | c .&. 0x80 == 0     = 1 -- 0___ ____
  | c .&. 0xE0 == 0xC0  = 2 -- 110_ ____
  | c .&. 0xF0 == 0xE0  = 3 -- 1110 ____
  | otherwise           = 4 -- 1111 ____

-- Computes how many utf8 codepoints are in string
utf8StringLen :: Int -> CString -> IO Int
utf8StringLen len str = run 0 str
  where
    end = str `plusCharPtr` len
    run total s
      | s >= end  = return total
      | otherwise = do
        c <- peek s
        run (total + 1) (s `plusCharPtr` utf8CodepointLen c)

-- Function callback used for `onig_foreach_name`
-- Receives in it's argument a pointer to an empty string array of size #captureNames * 2 * sizeOf(char*)
-- Fills the array with the start and end pointer of each capture name
captureNameIter :: NameIter (Ptr CString)
captureNameIter name nameEnd _ _ _ namesPtrPtr = unsafePerformIO $ do
  namesPtr <- peek namesPtrPtr
  poke (namesPtr `plusPtrPtr` 0) name
  poke (namesPtr `plusPtrPtr` 1) nameEnd
  poke namesPtrPtr $ namesPtr `plusPtrPtr` 2
  return 0

-- Build a map of capture group index to it's name (if exists)
captureNames :: Ptr Onig -> FunPtr (NameIter (Ptr CString)) -> IO (HashMap Int ByteString)
captureNames reg nameIter = do
  captureNameCount <- c_onig_number_of_names reg
  -- Allocate the captures names array
  allocaBytes (fromIntegral captureNameCount * 2 * sizeOfPtr) $ \namesPtr -> do
    -- We wrap it in another ptr so we can then move it inside the name iterator
    alloca $ \namesPtrPtr -> do
      poke namesPtrPtr namesPtr
      alloca $ \numListPtr -> do
        _ <- c_onig_foreach_name reg nameIter namesPtrPtr
        buildCapturesMap numListPtr namesPtr Map.empty captureNameCount
        where
          buildCapturesMap numListPtr names capturesMap namesLeft =
            if namesLeft <= 0
            then return capturesMap
            else do
              -- For each name we have the name string start ptr and then the end ptr
              nameStr <- peek names
              nameEnd <- peek $ names `plusPtrPtr` 1
              name <- packCStringLen (nameStr, nameEnd `minusPtr` nameStr)
              numCount <- c_onig_name_to_group_numbers reg nameStr nameEnd numListPtr
              numList <- peek numListPtr
              -- Each name has multimes idx that we insert into the map
              newMap <- foldM (\m i -> do
                  num <- peek (numList `plusIntPtr` i)
                  return $ Map.insert (fromIntegral (num-1)) name m
                ) capturesMap $ take (fromIntegral numCount) [0..]
              -- We continue iteration from the next name
              buildCapturesMap numListPtr (names `plusPtrPtr` 2) newMap (namesLeft-1)

buildCapture :: CString -> (Maybe ByteString, (Int, Int)) -> IO MatchCapture
buildCapture s (name, (beg, end)) =
  if beg == end
  then
    -- Empty capture
    if beg == -1
    then
      -- Didn't match
      return (Nothing, name)
    else do
      -- Empty match
      offset <- utf8StringLen beg s
      let string = ""
      let length = 0
      return (Just (offset, length, string), name)
  else do
    let realLen   = end - beg
    let matchStr  = s `plusCharPtr` beg
    offset  <- utf8StringLen beg s
    length  <- utf8StringLen realLen matchStr
    string  <- packCStringLen (matchStr, realLen)
    return (Just (offset, length, string), name)

----------------------------------- FFI Utils -----------------------------------------

-- `sizeOf` function family
sizeOfInt :: Int
sizeOfInt = sizeOf (undefined :: CInt)

sizeOfChar :: Int
sizeOfChar = sizeOf (undefined :: CChar)

sizeOfPtr :: Int
sizeOfPtr = sizeOf (undefined :: Ptr a)

-- `plusPtr` function family
plusIntPtr :: Ptr CInt -> Int -> Ptr CInt
plusIntPtr ptr n = ptr `plusPtr` (n * sizeOfInt)

plusCharPtr :: Ptr CChar -> Int -> Ptr CChar
plusCharPtr ptr n = ptr `plusPtr` (n * sizeOfChar)

plusPtrPtr :: Ptr (Ptr a) -> Int -> Ptr (Ptr a)
plusPtrPtr ptr n = ptr `plusPtr` (n * sizeOfPtr)

-- To create a function pointer of a haskell function callable from C, we must wrap it in a FunPtr
-- The returned function pointer must be released with `freeHaskellFunPtr` when no longer required
foreign import ccall "wrapper"
  nameIterToFunPtr :: NameIter a -> IO (FunPtr (NameIter a))

--------------------------- Foreign Oniguruma Functions -------------------------------
-- Oniguruma API Docs; https://github.com/kkos/oniguruma/blob/master/doc/API

-- Oniguruma UTF-8 encoding
foreign import capi unsafe "oniguruma.h &OnigEncodingUTF8"
  c_onig_encoding_utf8 :: Ptr OnigEncoding

-- Oniguruma Perl NG regex syntax
foreign import capi unsafe "oniguruma.h &OnigSyntaxPerl_NG"
  c_onig_syntax_perl_ng :: Ptr OnigSyntax

{-
# int onig_new(regex_t** reg, const UChar* pattern, const UChar* pattern_end,
            OnigOptionType option, OnigEncoding enc, OnigSyntaxType* syntax,
            OnigErrorInfo* err_info)

  Create a regex object.

  return value
  normal: ONIG_NORMAL == 0
  error:  error code < 0

  arguments
  1 reg:          return regex object's address.
  2 pattern:      regex pattern string.
  3 pattern_end:  terminate address of pattern. (pattern + pattern length)
  4 option:       compile time options.
  5 enc:          character encoding.
  6 syntax:       address of pattern syntax definition.
  7 err_info:     address for return optional error info.
                  Use this value as 3rd argument of onig_error_code_to_str().
-}
-- We have to use ccall because of GHC bug: https://gitlab.haskell.org/ghc/ghc/-/issues/15531
-- Fails on Ptr (Ptr Any)
foreign import ccall unsafe "oniguruma.h onig_new"
  c_onig_new :: Ptr (Ptr Onig) -> CString -> CString -> RegexOpt -> Ptr OnigEncoding -> Ptr OnigSyntax -> Ptr OnigErrorInfo -> IO CInt

{-
# OnigRegion* onig_region_new(void)

  Create a region.
-}
foreign import capi unsafe "oniguruma.h onig_region_new"
  c_onig_region_new :: IO (Ptr OnigRegion)

{-
# int onig_search(regex_t* reg, const UChar* str, const UChar* end, const UChar* start,
                   const UChar* range, OnigRegion* region, OnigOptionType option)

  Search string and return search result and matching region.
  Do not pass invalid byte string in the regex character encoding.

  return value
  normal:    match position offset (i.e.  p - str >= 0)
  not found: ONIG_MISMATCH (< 0)
  error:     error code    (< 0)

  arguments
  1 reg:    regex object
  2 str:    target string
  3 end:    terminate address of target string
  4 start:  search start address of target string
  5 range:  search terminate address of target string
    in forward search  (start <= searched string < range)
    in backward search (range <= searched string <= start)
  6 region: address for return group match range info (NULL is allowed)
  7 option: search time option
-}
foreign import capi unsafe "oniguruma.h onig_search"
  c_onig_search :: Ptr Onig -> CString -> Ptr CChar -> Ptr CChar -> Ptr CChar -> Ptr OnigRegion -> RegexOpt -> IO CInt

{-
# int onig_foreach_name(regex_t* reg,
          int (*func)(const UChar*, const UChar*, int,int*,regex_t*,void*),
          void* arg)

  Iterate function call for all names.

  return value
  normal: 0
  error:  return value of callback function

  arguments
  1 reg:     regex object.
  2 func:    callback function.
             func(name, name_end, <number of groups>, <group number's list>,
                  reg, arg);
             if func does not return 0, then iteration is stopped.
  3 arg:     argument for func.
-}
foreign import capi unsafe "oniguruma.h onig_foreach_name"
  c_onig_foreach_name :: Ptr Onig -> FunPtr (NameIter a) -> Ptr a -> IO CInt

{-
# int onig_number_of_names(regex_t* reg)

  Return the number of names defined in the pattern.
  Multiple definitions of one name is counted as one.

  arguments
  1 reg:     regex object.
-}
foreign import capi unsafe "oniguruma.h onig_number_of_names"
  c_onig_number_of_names :: Ptr Onig -> IO CInt

{-
# int onig_name_to_group_numbers(regex_t* reg, const UChar* name, const UChar* name_end,
                                  int** num_list)

  Return the group number list of the name.
  Named subexp is defined by (?<name>....).

  return value
  normal: number of groups for the name.
          (ex. /(?<x>..)(?<x>..)/  ==>  2)
  name not found: -1

  arguments
  1 reg:       regex object.
  2 name:      group name.
  3 name_end:  terminate address of group name.
  4 num_list:  return list of group number.
-}
-- We have to use ccall because of GHC bug: https://gitlab.haskell.org/ghc/ghc/-/issues/15531
-- Fails on Ptr (Ptr Any)
foreign import ccall unsafe "oniguruma.h onig_name_to_group_numbers"
  c_onig_name_to_group_numbers :: Ptr Onig -> CString -> Ptr CChar -> Ptr (Ptr CInt) -> IO CInt

{-
# int onig_error_code_to_str(UChar* err_buf, int err_code, ...)

  Get error message string.
  If this function is used for onig_new(),
  don't call this after the pattern argument of onig_new() is freed.

  return value
  normal: error message string length

  arguments
  1 err_buf:              error message string buffer.
                          (required size: ONIG_MAX_ERROR_MESSAGE_LEN)
  2 err_code:             error code returned by other API functions.
  3 err_info (optional):  error info returned by onig_new().
-}
foreign import capi unsafe "oniguruma.h onig_error_code_to_str"
  c_onig_error_code_to_str :: CString -> CInt -> Ptr OnigErrorInfo -> IO CInt

{-
# void onig_free(regex_t* reg)

  Free memory used by regex object.

  arguments
  1 reg: regex object.
-}
foreign import capi unsafe "oniguruma.h &onig_free"
  c_onig_free :: FinalizerPtr Onig

{-
# void onig_region_free(OnigRegion* region, int free_self)

  Free memory used by region.

  arguments
  1 region:    target region
  2 free_self: [1: free all, 0: free memory used in region but not self]
-}
foreign import capi unsafe "oniguruma.h onig_region_free"
  c_onig_region_free :: Ptr OnigRegion -> CInt -> IO ()
