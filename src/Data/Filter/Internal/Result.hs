module Data.Filter.Internal.Result (
  FilterRet (..)
, retOk
, retErr
, applyByOk

, FilterResult
, resultOk
, resultErr
, mapRet
, filterRet
, foldMRet
, mapMRet
, concatMapMRet

, concatMapM
) where

import Data.Json (Json)

import Data.Text (Text)
import Control.Monad (liftM, ap, foldM, (<=<))
import Data.Foldable (fold)

data FilterRet a
  = Ok      a                 -- Valid value
  | Err     Text              -- Error with message
  | Break   Text              -- Backtrack to label
  | Halt    Int (Maybe Json)  -- Stop excecution with exit code and optional value
  | Stderr  Json              -- Print message on stderr and continue execution
  | Debug   Json              -- Print debug message on stderr and continue execution
  deriving (Eq, Show)

instance Functor FilterRet where
  fmap = liftM

instance Applicative FilterRet where
  pure = Ok
  (<*>) = ap

instance Monad FilterRet where
  m >>= k = case m of
    Ok a          -> k a
    Err msg       -> Err msg
    Break label   -> Break label
    Halt c msg    -> Halt c msg
    Stderr msg    -> Stderr msg
    Debug msg     -> Debug msg

retOk :: Monad m => a -> m (FilterRet a)
retOk = return . Ok

retErr :: Monad m => Text -> m (FilterRet a)
retErr = return . Err

applyByOk :: (a -> t) -> (FilterRet b -> t) -> FilterRet a -> t
applyByOk ok else' ret = case ret of
  Ok a        -> ok a
  Err msg     -> else' $ Err msg
  Break label -> else' $ Break label
  Halt c msg  -> else' $ Halt c msg
  Stderr msg  -> else' $ Stderr msg
  Debug msg   -> else' $ Debug msg

applyByInterrupt :: (FilterRet a -> t) -> (FilterRet a -> t) -> FilterRet a -> t
applyByInterrupt interrupt else' ret = case ret of
  Ok a        -> else'      $ Ok a
  Err msg     -> interrupt  $ Err msg
  Break label -> interrupt  $ Break label
  Halt c msg  -> interrupt  $ Halt c msg
  Stderr msg  -> else'      $ Stderr msg
  Debug msg   -> else'      $ Debug msg

applyByOkAndInterrupt :: (a -> t) -> (FilterRet b -> t) -> (FilterRet b -> t) -> FilterRet a -> t
applyByOkAndInterrupt ok interrupt else' ret = case ret of
  Ok a        -> ok a
  Err msg     -> interrupt  $ Err msg
  Break label -> interrupt  $ Break label
  Halt c msg  -> interrupt  $ Halt c msg
  Stderr msg  -> else'      $ Stderr msg
  Debug msg   -> else'      $ Debug msg

type FilterResult a = [FilterRet a]

resultOk :: Monad m => a -> m (FilterResult a)
resultOk = return . (:[]) . Ok

resultErr :: Monad m => Text -> m (FilterResult a)
resultErr = return . (:[]) . Err

mapRet :: (a -> FilterRet b) -> FilterResult a -> FilterResult b
mapRet f = foldr (\x ret -> applyByOkAndInterrupt (applyByInterrupt (:[]) (:ret) . f) (:[]) (:ret) x) []

filterRet :: (a -> Bool) -> FilterResult a -> FilterResult a
filterRet f = foldr (\x ret -> applyByOkAndInterrupt (\a -> if f a then x : ret else ret) (:[]) (:ret) x) []

foldMRet :: Monad m => (b -> a -> m (FilterRet b)) -> FilterRet b -> FilterResult a -> m (FilterRet b)
foldMRet f = foldM (\a b -> applyByOk (\ret -> applyByOk (f ret) return b) return a)

mapMRet :: Monad m => (a -> m (FilterRet b)) -> FilterResult a -> m (FilterResult b)
mapMRet f = foldr (\x mret -> do
    ret <- mret
    applyByOkAndInterrupt (applyByInterrupt (return . (:[])) (return . (:ret)) <=< f) (return . (:[])) (return . (:ret)) x
  ) (return [])

mapMRet' :: Monad m => (a -> m b) -> FilterResult a -> m (FilterResult b)
mapMRet' f = mapM $ applyByOk (fmap Ok . f) return

concatRet :: FilterResult (FilterResult a) -> FilterResult a
concatRet = foldr (\x ret -> applyByOk (foldr (applyByInterrupt (const . (:[])) (:)) ret) (:ret) x) []

concatMapMRet :: Monad m => (a -> m (FilterResult b)) -> FilterResult a -> m (FilterResult b)
concatMapMRet f = fmap concatRet . mapMRet' f

-- Utils --
concatMapM :: (Monoid b, Traversable t, Applicative f) => (a -> f b) -> t a -> f b
concatMapM f = fmap fold . traverse f
