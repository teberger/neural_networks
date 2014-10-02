module Main where

main :: IO ()
main = return ()

type Epoch a b = [Sample a b]
data Sample a b = Sample { datum :: a,
                           classification :: Enum b}
