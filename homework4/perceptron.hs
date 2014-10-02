{-# LANGUAGE RankNTypes #-}
module Main where

type Epoch a b = [Sample a b]
data Sample a b = Sample { datum :: a,
                           classification :: (Enum b => b)}

eta_params :: [Double]
eta_params = [0,0.05..1]

main :: IO ()
main = print eta_params

getData :: Enum b => Sample a b -> a
getData s = datum s

getClass :: Enum b =>  Sample a b -> b
getClass s = classification s
