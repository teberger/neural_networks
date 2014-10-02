module Main where

type Epoch a b = [Sample a b]
data Sample a b = Sample { datum :: a,
                           classification :: Enum b}

main :: IO ()
main = return ()

getData :: Sample a b -> a
getData s = datum s

getClass :: Sample a b -> b
getClass s = classification s
