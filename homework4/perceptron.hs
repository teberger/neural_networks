{-# LANGUAGE RankNTypes #-}
module Main where

import System.Environment
import System.Random
import System.IO
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.List

type Epoch a b = [Sample a b]
data Sample a b = Sample { datum :: (Num a) => V.Vector a,
                           classification :: (Enum b) => b}

instance (Num a, Enum b, Show a, Show b) => Show (Sample a b) where
  show (Sample a b) = (show a) ++ "\n" ++ "class: " ++ (show b)

data Perceptron a = Perceptron { w :: (Num a) => V.Vector a,
                                 y :: Double -> Double
                               }

data Classes = C1 | C2 deriving (Enum, Show)

eta_params :: [Double]
eta_params = [0.0 , 0.05, 0.1,
              0.15, 0.2 , 0.25,
              0.3 , 0.35, 0.4,
              0.45, 0.5
             ]
             
type Length = Int
type Seed = Int
type WeightGenerator = Seed -> Length -> [Double]
            
main :: IO ()
main = do
  args <- getArgs
  let train_c1 = args !! 0
      train_c2 = args !! 1
      test_c1 = args !! 3
      test_c2 = args !! 4

  c1Data <- constructData C1 =<< hGetContents =<< openFile train_c1 ReadMode
  c1Test <- constructData C1 =<< hGetContents =<< openFile train_c2 ReadMode
  c2Data <- constructData C2 =<< hGetContents =<< openFile train_c2 ReadMode
  c2Test <- constructData C2 =<< hGetContents =<< openFile train_c2 ReadMode

  let trainingData = c1Data ++ c2Data
      testingData = c1Test ++ c2Test
      
  print $ datum $ c1Data !! 1

constructData :: Classes -> String -> IO [Sample Double Classes]
constructData clazz file = return constructSamples
  where samples = lines file
        pairs = map words samples
        constructVects = map (V.fromList . (drop 1) . (map read)) pairs
        constructSamples =  map (\x -> Sample x clazz) constructVects

weights :: [WeightGenerator]
weights =  [all_ones, random_weights]

all_ones :: WeightGenerator
all_ones _ l = [1 | x <- [1..l]]

random_weights :: WeightGenerator
random_weights seed length = let r = mkStdGen seed in
                              map (/ (fromIntegral . snd . genRange $ r)) .
                                  (rNumber r) $ all_ones seed length

rNumber :: (RandomGen g)=> g -> [Double] -> [Double]
rNumber _ [] = []
rNumber gen (x:xs) = (x * (fromIntegral n)) : (rNumber g' xs)
  where (n, g') = next gen

sampleDatum :: (Num a) => Sample a b -> V.Vector a
sampleDatum s = datum s

getClass :: Enum b =>  Sample a b -> b
getClass s = classification s

shuffleList :: RandomGen g => g -> [a] -> [a]
shuffleList gen = fst . (fisherYates gen)

fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen
 
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
      where
            toElems (x, y) = (Map.elems x, y)
            numerate = zip [1..] 
            initial x gen = (Map.singleton 0 x, gen)

