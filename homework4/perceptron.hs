module Main where

main :: IO ()
main = return ()

type Epoch a = [Sample a]
data Sample a = Sample { datum :: a,
                         classification :: Enum b}
