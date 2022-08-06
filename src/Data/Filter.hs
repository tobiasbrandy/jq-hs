module Data.Filter (
  Filter (..)
) where

data Filter
  = Identity
  | Null
  deriving (Eq, Show)
