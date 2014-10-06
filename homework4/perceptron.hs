{-# LANGUAGE RankNTypes #-}
module Main where

import System.Environment
import System.Random
import System.IO
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.List

type Error = Double
type Epoch a b = [Sample a b]

data Sample a b = Sample { datum :: (Num a) => V.Vector a,
                           classification :: (Enum b) => b
                         }
                  
data Classes = C1 | C2 deriving (Enum, Show, Eq)

instance (Num a, Enum b, Show a, Show b) => Show (Sample a b) where
  show (Sample a b) = (show a) ++ "\n" ++ "class: " ++ (show b)

data Perceptron a = Perceptron { w :: (Num a) => V.Vector a,
                                 y :: Double -> Double
                               }

eta_params :: [Double]
eta_params = [0.05, 0.1 , 0.15,
              0.2 , 0.25, 0.3 ,
              0.35, 0.4 , 0.45,
              0.5
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

  let trainingData = c1Data ++ c2Data :: [Sample Double Classes]
      testingData = c1Test ++ c2Test :: [Sample Double Classes]

  hOutput <- openFile "eta_variation.csv" WriteMode
  hSetBuffering hOutput LineBuffering
  hPutStr hOutput "eta epoch training_error testing_error"
  let p = Perceptron (V.fromList (all_ones 1 2)) syn
  mapM_ (converge hOutput 0 p trainingData testingData) eta_params

syn :: Double -> Double
syn x = if x < 0 then -1 else 1

converge :: Handle -> Int -> Perceptron Double -> Epoch Double Classes -> Epoch Double Classes -> Double -> IO ()
converge file num p train test eta = do
  putStrLn $ "Beginning epoch: " ++ (show num)
  let (p', e) = learnEpoch p train eta
      testError = testEpoch p test eta
  hPutStrLn file $ (show eta) ++ " " ++
                   (show num) ++ " " ++
                   (show e) ++ " " ++
                   (show testError)
  if (e == 0) then return () else converge file (num + 1) p' train test eta
  
testEpoch :: Perceptron Double -> Epoch Double Classes -> Double -> Error
testEpoch p ls eta = foldl (\e sample -> e + (test eta p sample)) 0 ls

learnEpoch :: Perceptron Double -> Epoch Double Classes -> Double -> (Perceptron Double, Error)
learnEpoch p ls eta = foldl (\(p, e) sample ->
                            let (p', e') = learn eta p sample
                            in (p', e + e'))
                          (p, 0)
                          ls

test :: Double -> Perceptron Double -> Sample Double Classes -> Error
test eta p@(Perceptron xs y) s@(Sample v c) = if c == clazz then 0 else 1
  where (clazz, sign) = classify p s

learn :: Double -> Perceptron Double -> Sample Double Classes -> (Perceptron Double, Error)
learn eta p@(Perceptron xs y) s@(Sample v c) = if c == clazz then (p, 0) else (Perceptron (deltaW xs sign eta v) y, 2)
  where (clazz, sign) = classify p s 

deltaW :: V.Vector Double -> Error -> Double -> V.Vector Double -> V.Vector Double
deltaW xs sign eta vs = V.zipWith (+) xs (V.map (* (eta * sign)) vs)

classify :: Perceptron Double -> Sample Double Classes -> (Classes, Error)
classify (Perceptron xs y) (Sample v c) = if output /= 0 then (switch c, sign) else (c, sign)
  where output = y (dot xs v)
        sign = if output < 0 then -1 else 1

switch c = if c == C1 then C2 else C1

dot :: Num a => V.Vector a -> V.Vector a -> a
dot a b = (V.foldl (+) 0) (V.zipWith (*) a b)

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

