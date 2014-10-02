{-# LANGUAGE RankNTypes #-}
module Main where

import System.Environment

type Epoch a b = [Sample a b]
data Sample a b = Sample { datum :: a,
                           classification :: (Enum b => b)}

data Classes = C1 | C2 deriving Enum

eta_params :: [Double]
eta_params = [0.0 , 0.05, 0.1,
              0.15, 0.2 , 0.25,
              0.3 , 0.35, 0.4,
              0.45, 0.5
             ]

type Length = Int
type Seed = Int

weights :: [Seed -> Length -> [Double]]
weights = [ones, random]

ones :: Seed -> Length -> [Double]
ones = undefined

random :: Seed -> Length -> [Double]
random = undefined

main :: IO ()
main = do
  args <- getArgs

getData :: Enum b => Sample a b -> a
getData s = datum s

getClass :: Enum b =>  Sample a b -> b
getClass s = classification s
