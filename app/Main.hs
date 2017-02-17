module Main where

import Lib


data Sym = A | B deriving Show

rew :: Sym -> [Sym]
rew A = [A,B]
rew B = [A]

sampleSystem :: LSys Sym
sampleSystem = LSys [B] rew

main :: IO ()
main = do
  let steps = map printSys . scanl (\acc _ -> step acc) sampleSystem $ [1..10]
  mapM_ putStrLn steps
