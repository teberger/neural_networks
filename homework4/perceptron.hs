{-# LANGUAGE RankNTypes #-}
module Main where

import System.Environment
import Data.List
import System.Random

type Epoch a = [Sample a]
data Sample a = Sample { datum :: a,
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
type WeightGenerator = Seed -> Length -> IO [Double]

weights :: [WeightGenerator]
weights =  [all_ones, random_weights]

all_ones :: WeightGenerator
all_ones _ l = return $ [1 | x <- [1..l]]

random_weights :: WeightGenerator
random_weights = undefined

shuffleList :: RandomGen r -> [a] -> [a]
shuffleList = undefined

main :: IO ()
main = do
 args <- getArgs
 print =<< all_ones 1 10

getData :: Sample a -> a
getData s = datum s

getClass :: Enum b =>  Sample a -> b
getClass s = classification s
