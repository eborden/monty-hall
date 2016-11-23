
### Imports/Pragmas
```haskell
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad.Writer     (Writer, runWriter
                                          , execWriter, tell)
import           Control.Parallel         (pseq, par)
import           Data.Foldable            (foldl', fold)
import           Data.Monoid              ((<>))
import           Data.Vector              (Vector)
import           Data.Vector.Generic      ((!))
import qualified Data.Vector.Generic      as G
import qualified Data.Vector.Unboxed      as U
import           System.Environment       (getArgs)
import           System.Random            (StdGen, mkStdGen
                                          , getStdGen, randomR)
```



### Game Types
```haskell
data Prize = Car | Goat

data Option = Door1 | Door2 | Door3
  deriving (Eq)

data Doors = Doors !Prize !Prize !Prize

data Result = Result !Int !Int
  deriving (Show)

instance Monoid Result where
  mempty = Result 0 0
  Result x y `mappend` Result x' y' = Result (x + x') (y + y')

type PickDoor = Doors -> Prize
```



### Game Logic
```haskell
playAlgo :: StdGen -> Writer Result StdGen
playAlgo gen = do
  let (door,   gen')   = takeRand gen doors
      (stay,   gen'')  = takeRand gen' options
      (reveal, gen''') = takeRand gen'' . G.filter (/= stay) $ monty door
      swap = G.head $ G.filter (\x -> x /= reveal && x /= stay) options

  ifWin markStay $ openDoor stay door
  ifWin markSwap $ openDoor swap door

  pure gen'''

ifWin :: Writer Result () -> Prize -> Writer Result ()
ifWin action prize = case prize of
  Car -> action
  Goat -> pure ()

markStay, markSwap :: Writer Result ()
markStay = tell $ Result 1 0
markSwap = tell $ Result 0 1

door1, door2, door3 :: PickDoor
door1 (Doors x _ _) = x
door2 (Doors _ x _) = x
door3 (Doors _ _ x) = x

options :: Vector Option
options= G.fromList [Door1, Door2, Door3]

openDoor :: Option -> PickDoor
openDoor x = case x of
  Door1 -> door1
  Door2 -> door2
  Door3 -> door3

doors :: Vector Doors
doors = G.fromList [ Doors Car Goat Goat
                   , Doors Goat Car Goat
                   , Doors Goat Goat Car
                   ]

monty :: Doors -> Vector Option
monty ds = case ds of
  (Doors Goat Goat Car) -> goatGoatCar
  (Doors Goat Car Goat) -> goatCarGoat
  (Doors Car Goat Goat) -> carGoatGoat
  _ -> error "monty: impossible: Non conforming door."

goatGoatCar = G.fromList [Door1, Door2]
goatCarGoat = G.fromList [Door1, Door3]
carGoatGoat = G.fromList [Door2, Door3]

takeRand :: G.Vector v a => StdGen -> v a -> (a, StdGen)
takeRand gen xs =
  let (i, gen') = randomR (0, G.length xs - 1) gen
  in (xs ! i, gen')
```



### Parallel Folds
```haskell
parFold :: Monoid a => [a] -> a
parFold [] = mempty
parFold [x] = x
parFold xs = (ys `par` zs) `pseq` (ys <> zs)
  where (ys', zs') = splitAt (length xs `div` 2) xs
        ys = parFold ys'
        zs = parFold zs'

vparFoldMap :: (G.Vector v a, Monoid b) => (a -> b) -> v a -> b
vparFoldMap f xs
  | G.null xs = mempty
  | G.length xs == 1 = f $ G.head xs
  | otherwise = 
      let (ys', zs') = G.splitAt (G.length xs `div` 2) xs
          ys = vparFoldMap f ys'
          zs = vparFoldMap f zs'
      in (ys `par` zs) `pseq` (ys <> zs)
```



### Various Execution Strategies
```haskell
play :: StdGen -> Result
play = execWriter . playAlgo

runFold :: Int -> IO Result
runFold iterations = do
  gen <- getStdGen
  pure . snd $ foldl' play' (gen, mempty) [1..iterations]
  where
    play' (gen, !acc) _ = fmap (<> acc) . runWriter $ playAlgo gen

runSeed :: Int -> Result
runSeed iterations = foldl' play' mempty [1..iterations]
  where
    play' acc i = (<> acc) . play $ mkStdGen i

runConcurrent :: Int -> IO Result
runConcurrent iterations = do
  fmap fold $ mapConcurrently (pure . play . mkStdGen) [1..iterations]

runPar :: Int -> Result
runPar iterations =
  parFold $ fmap (play . mkStdGen) [1..iterations]

runParVector :: Int -> Result
runParVector iterations =
  vparFoldMap (play . mkStdGen) $ U.enumFromN 1 iterations

percentize :: Int -> Int -> String
percentize t x = show (fromIntegral x / (fromIntegral t) * 100) <> "%"
```



### Making it Runnable
```haskell
main :: IO ()
main = do

  i:r:_ <- getArgs
  let iterations = read i

  Result stay swap <- case r of
    "f" -> runFold iterations
    "s" -> pure $ runSeed iterations
    "c" -> runConcurrent iterations
    "p" -> pure $ runPar iterations
    "pv" -> pure $ runParVector iterations
    _ -> error "usage: ITERATIONS [p|f|s|c]"

  putStrLn $ "Stay: " <> percentize iterations stay
  putStrLn $ "Swap: " <> percentize iterations swap
```
