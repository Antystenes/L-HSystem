module Lib
    ( LSys(..),
      step
    ) where

data LSys sym = LSys {
  axiom :: [sym],
  rewr  :: sym -> [sym]}

step :: LSys a -> LSys a
step (LSys ax r) = LSys (ax >>= r) r


instance (Show sym) => Show (LSys sym) where
  show = show . axiom
