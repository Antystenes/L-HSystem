module Main where

import LHsys


data Sym = A | B deriving Show

rew :: Sym -> [Sym]
rew A = [A,B]
rew B = [A]

sampleSystem :: LSys Sym
sampleSystem = LSys [B] rew

data StackSyms = Go | L | R | S (LSys StackSyms)

instance Show StackSyms where
  show Go       = "Go"
  show L        = "L"
  show R        = "R"
  show (S sys)  = show sys

rewStack :: StackSyms -> [StackSyms]
rewStack Go = [Go, branch [L, Go], Go, branch [R, Go], Go ]
  where branch l = S (LSys l rewStack)
rewStack (S branch) = [S (step branch)]
rewStack x = [x]

sampleStackSys :: LSys StackSyms
sampleStackSys = LSys [Go] rewStack

main :: IO ()
main = do
  let steps = map show . scanl (\acc _ -> step acc) sampleStackSys $ [1..5]
  mapM_ putStrLn steps
