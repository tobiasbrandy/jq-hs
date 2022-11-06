module Data.Filter.Run 
( filterRunExp
, Run.filterRunModule
, FilterResult
, FilterRet (..)
, Run.FilterFunc
) where

import Prelude hiding (filter)

import qualified Data.Filter.Internal.Run as Run (filterRunExp, filterRunModule, FilterFunc)
import qualified Data.Filter.Internal.Result as Ret (FilterRet (..))

import Data.Filter (Filter)
import Data.Json (Json)

import Data.Text (Text)
import Data.HashMap.Internal.Strict (HashMap)
import Control.Monad (liftM, ap)

-- We uncouple internal representation from API
data FilterRet a
  = Ok      a                 -- Valid value
  | Err     Text              -- Error with message
  | Halt    Int (Maybe Json)  -- Stop excecution with exit code and optional message
  | Stderr  Json              -- Print message on stderr and continues execution
  | Debug   Json              -- Print debug message on stderr and continue execution
  deriving (Eq, Show)

instance Functor FilterRet where
  fmap = liftM

instance Applicative FilterRet where
  pure = Ok
  (<*>) = ap

instance Monad FilterRet where
  m >>= k = case m of
    Ok a        -> k a
    Err msg     -> Err msg
    Halt c msg  -> Halt c msg
    Stderr msg  -> Stderr msg
    Debug msg   -> Debug msg

toExternalRepr :: Ret.FilterRet a -> FilterRet a
toExternalRepr (Ret.Ok a)       = Ok a
toExternalRepr (Ret.Err msg)    = Err msg
toExternalRepr (Ret.Break _)    = error "Ret.Break has no external representation"
toExternalRepr (Ret.Halt c msg) = Halt c msg
toExternalRepr (Ret.Stderr msg) = Stderr msg
toExternalRepr (Ret.Debug msg)  = Debug msg

type FilterResult a = [FilterRet a]

filterRunExp :: HashMap (Text, Int) Run.FilterFunc -> Filter -> Json -> FilterResult Json
filterRunExp funcs filter json = map toExternalRepr $ Run.filterRunExp funcs filter json
