module LHsys
    ( LSys(..),
      step
    ) where

data LSys sym = LSys {
  inst  :: [sym],
  rewr  :: sym -> [sym]}

step :: LSys a -> LSys a
step (LSys i r) = LSys (i >>= r) r

instance (Show sym) => Show (LSys sym) where
  show = show . inst
