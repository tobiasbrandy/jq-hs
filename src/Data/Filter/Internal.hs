module Data.Filter.Internal (
  FilterRet (..)
, retOk
, retErr
, applyRet
, retToEither

, FilterResult
, resultOk
, resultErr
, resultHalt
, foldrRet
, mapRet
, foldMRet
, mapMRet
, concatMapMRet

-- Error handling
, jsonShowError

-- Utils
, concatMapM
) where

import Data.Json (Json(..))

import Data.Text (Text)
import Control.Monad (liftM, ap)
import Data.Foldable (fold)
import TextShow (showt)

data FilterRet a
  = Ok   a
  | Err  Text
  | Halt Text

instance Functor FilterRet where
  fmap = liftM

instance Applicative FilterRet where
  pure = Ok
  (<*>) = ap

instance Monad FilterRet where
  m >>= k = case m of
    Ok a        -> k a
    Err msg     -> Err msg
    Halt label  -> Halt label

retOk :: Monad m => a -> m (FilterRet a)
retOk = return . Ok

retErr :: Monad m => Text -> m (FilterRet a)
retErr = return . Err

retHalt :: Monad m => Text -> m (FilterRet a)
retHalt = return . Halt

applyRet :: (a -> t) -> (FilterRet b -> t) -> FilterRet a -> t
applyRet ok errOrHalt ret = case ret of
  (Ok a)        -> ok a
  (Err msg)     -> errOrHalt $ Err msg
  (Halt label)  -> errOrHalt $ Halt label

retToEither :: FilterRet a -> Either Text a
retToEither (Ok a)    = Right a
retToEither (Err msg) = Left msg
retToEither (Halt _)  = error "Interrupts cannot be mapped to either"

type FilterResult a = [FilterRet a]

resultOk :: Monad m => a -> m (FilterResult a)
resultOk = return . (:[]) . Ok

resultErr :: Monad m => Text -> m (FilterResult a)
resultErr = return . (:[]) . Err

resultHalt :: Monad m => Text -> m (FilterResult a)
resultHalt = return . (:[]) . Halt

foldrRetOnInt :: (FilterRet a -> t -> t) -> t -> t -> FilterResult a -> t
foldrRetOnInt f base halt = foldr go base
  where
    go h@(Halt _) _   = f h halt
    go x          ret = f x ret

foldrRet :: (FilterRet a -> t -> t) -> t -> FilterResult a -> t
foldrRet f base = foldrRetOnInt f base base

mapRet :: (a -> FilterRet b) -> FilterResult a -> FilterResult b
mapRet f = foldrRet go []
  where
    go (Ok a) ret = case f a of
      (Halt label)  -> [Halt label]
      other         -> other : ret
    go (Err msg)    ret = Err msg : ret
    go (Halt label) ret = Halt label : ret

foldMRet :: Monad m => (b -> a -> m (FilterRet b)) -> FilterRet b -> FilterResult a -> m (FilterRet b)
foldMRet f = foldMRet' run
  where
    run _         (Err msg)     = retErr msg
    run _         (Halt label)  = retHalt label
    run (Ok ret)  (Ok x)        = f ret x
    run (Err _)   _             = invalidStateErr "Err"
    run (Halt _)  _             = invalidStateErr "Interrupt"

    invalidStateErr retCase = error $ "ret=" <> retCase <> ": Should never happen. Case 1 already catches this case. We want to short circuit on errors."

foldMRet' :: Monad m => (t -> FilterRet a -> m t) -> t -> FilterResult a -> m t
foldMRet' f base xs = foldrRet go return xs base
  where go x fret acum = fret =<< f acum x

mapMRet :: Monad m => (a -> m (FilterRet b)) -> FilterResult a -> m (FilterResult b)
mapMRet f = foldMRet' go []
  where
    go ret (Ok a) = do
      b <- f a
      case b of
        h@(Halt _)  -> return [h]
        other       -> return $ other : ret
    go ret (Err msg)    = return $ Err msg : ret
    go _   (Halt label) = resultHalt label

mapMRet' :: Monad m => (a -> m b) -> FilterResult a -> m (FilterResult b)
mapMRet' f = sequence . foldrRet ((:) . applyRet (fmap Ok . f) return) []

concatRet :: FilterResult (FilterResult a) -> FilterResult a
concatRet = foldrRet (\x ret -> applyRet (foldrRetOnInt (:) ret []) (:ret) x) []

concatMapMRet :: Monad m => (a -> m (FilterResult b)) -> FilterResult a -> m (FilterResult b)
concatMapMRet f = fmap concatRet . mapMRet' f

-- Error handling --
-- TODO(tobi): Agregar cantidad maxima de caracteres y luego ...
jsonShowError :: Json -> Text
jsonShowError json@(Number  _)  = "number ("  <> showt json <> ")"
jsonShowError json@(String  _)  = "string ("  <> showt json <> ")"
jsonShowError json@(Bool    _)  = "boolean (" <> showt json <> ")"
jsonShowError json@(Object  _)  = "object ("  <> showt json <> ")"
jsonShowError json@(Array   _)  = "array ("   <> showt json <> ")"
jsonShowError json@Null         = "null ("    <> showt json <> ")"

-- Utils --
concatMapM :: (Monoid b, Traversable t, Applicative f) => (a -> f b) -> t a -> f b
concatMapM f = fmap fold . traverse f
