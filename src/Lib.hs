module Lib
    ( LSys(..),
      step,
      printSys
    ) where

data LSys sym = LSys {
  axiom :: [sym],
  rewr  :: sym -> [sym]}

step :: LSys a -> LSys a
step (LSys ax r) = (LSys (ax >>= r) r)


printSys :: (Show sym) => LSys sym -> String
printSys = show . axiom


someFunc :: IO ()
someFunc = putStrLn "someFunc"
