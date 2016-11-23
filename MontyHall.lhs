
### Imports/Pragmas
```haskell
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad.Writer     (Writer, runWriter
                                          , execWriter, tell)
import           Control.Parallel         (pseq, par)
import           Control.Concurrent       (getNumCapabilities)
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



### Synchronous Execution
```haskell
play :: StdGen -> Result
play = execWriter . playAlgo

runFold :: Int -> IO Result
runFold iterations = do
  gen <- getStdGen
  pure . snd $ foldl' play' (gen, mempty) [1..iterations]
  where
    play' (gen, !acc) _ = fmap (<> acc) . runWriter $ playAlgo gen
```



### Synchronous Execution with Simulated Rand
```haskell
runSeed :: Int -> Result
runSeed iterations = foldl' play' mempty [1..iterations]
  where
    play' acc i = (<> acc) . play $ mkStdGen i
```



### Concurrent Execution
```haskell
runConcurrent :: Int -> IO Result
runConcurrent iterations = do
  fmap fold $ mapConcurrently (pure . play . mkStdGen) [1..iterations]
```



### Parallel Execution
```haskell
runPar :: Int -> Result
runPar iterations =
  parFoldMap (play . mkStdGen) [1..iterations]

parFoldMap :: Monoid b => (a -> b) -> [a] -> b
parFoldMap f [] = mempty
parFoldMap f [x] = f x
parFoldMap f xs = (ys `par` zs) `pseq` (ys <> zs)
  where (ys', zs') = splitAt (length xs `div` 2) xs
        ys = parFoldMap f ys'
        zs = parFoldMap f zs'
```

```
$ time ./.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/monty-hall-exe/monty-hall-exe 100000 p +RTS -s
Stay: 33.329%
Swap: 66.671%
     429,611,008 bytes allocated in the heap
      37,931,376 bytes copied during GC
       4,248,504 bytes maximum residency (7 sample(s))
         761,704 bytes maximum slop
              15 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       253 colls,   253 par    0.229s   0.096s     0.0004s    0.0104s
  Gen  1         7 colls,     6 par    0.066s   0.031s     0.0045s    0.0107s

  Parallel GC work balance: 14.84% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 100534 (53 converted, 0 overflowed, 0 dud, 96016 GC'd, 4465 fizzled)

  INIT    time    0.000s  (  0.003s elapsed)
  MUT     time    0.514s  (  0.166s elapsed)
  GC      time    0.296s  (  0.127s elapsed)
  EXIT    time    0.001s  (  0.001s elapsed)
  Total   time    0.811s  (  0.297s elapsed)

  Alloc rate    835,493,986 bytes per MUT second

  Productivity  63.5% of total user, 173.6% of total elapsed

gc_alloc_block_sync: 3414
whitehole_spin: 0
gen[0].sync: 1
gen[1].sync: 45

real    0m0.302s
user    0m0.811s
sys     0m0.192s

```


### Parallel Execution with Vectors
```haskell
runParVector :: Int -> Result
runParVector iterations =
  vparFoldMap (play . mkStdGen) $ U.enumFromN 1 iterations

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

```
$ time ./.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/monty-hall-exe/monty-hall-exe 100000 pv +RTS -s                                                   
Stay: 33.329%
Swap: 66.671%
     339,836,008 bytes allocated in the heap
       1,167,496 bytes copied during GC
         849,248 bytes maximum residency (2 sample(s))
          48,496 bytes maximum slop
               4 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       200 colls,   200 par    0.019s   0.008s     0.0000s    0.0016s
  Gen  1         2 colls,     1 par    0.000s   0.001s     0.0003s    0.0003s

  Parallel GC work balance: 58.21% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 100907 (84 converted, 0 overflowed, 0 dud, 95356 GC'd, 5467 fizzled)

  INIT    time    0.000s  (  0.002s elapsed)
  MUT     time    0.336s  (  0.094s elapsed)
  GC      time    0.019s  (  0.008s elapsed)
  EXIT    time    0.000s  (  0.001s elapsed)
  Total   time    0.355s  (  0.105s elapsed)

  Alloc rate    1,011,838,289 bytes per MUT second

  Productivity  94.7% of total user, 318.7% of total elapsed

gc_alloc_block_sync: 634
whitehole_spin: 0
gen[0].sync: 2
gen[1].sync: 1

real    0m0.110s
user    0m0.357s
sys     0m0.014s
```

### Parallel Execution with Capabilities
```haskell
chunk :: (G.Vector v a) => Int -> v a -> [v a]
chunk 0 _ = []
chunk 1 xs = [xs]
chunk i xs = let l = G.length xs `div` i
              in G.take l xs: chunk (i -1) (G.drop l xs)

runSplit :: Int -> IO Result
runSplit iterations = do
  n <- getNumCapabilities
  pure . parFoldMap (G.foldl f mempty) . chunk n $ U.enumFromN 1 iterations
  where
    f !acc i = mappend acc . play $ mkStdGen i
```

```
$ time ./.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/monty-hall-exe/monty-hall-exe 100000 sv +RTS -s
Stay: 33.329%
Swap: 66.671%
     301,797,976 bytes allocated in the heap
         616,912 bytes copied during GC
         852,856 bytes maximum residency (2 sample(s))
          49,272 bytes maximum slop
               4 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       165 colls,   165 par    0.010s   0.004s     0.0000s    0.0012s
  Gen  1         2 colls,     1 par    0.000s   0.001s     0.0003s    0.0004s

  Parallel GC work balance: 43.18% (serial 0%, perfect 100%)

  TASKS: 10 (1 bound, 9 peak workers (9 total), using -N4)

  SPARKS: 3 (3 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.007s  (  0.003s elapsed)
  MUT     time    0.317s  (  0.091s elapsed)
  GC      time    0.010s  (  0.005s elapsed)
  EXIT    time    0.001s  (  0.001s elapsed)
  Total   time    0.335s  (  0.100s elapsed)

  Alloc rate    953,457,732 bytes per MUT second

  Productivity  94.8% of total user, 318.2% of total elapsed

gc_alloc_block_sync: 269
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0

real    0m0.104s
user    0m0.336s
sys     0m0.021s
```



### Making it Runnable
```haskell
percentize :: Int -> Int -> String
percentize t x = show (fromIntegral x / (fromIntegral t) * 100) <> "%"

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
    "sv" -> runSplit iterations
    _ -> error "usage: ITERATIONS [p|f|s|c]"

  putStrLn $ "Stay: " <> percentize iterations stay
  putStrLn $ "Swap: " <> percentize iterations swap
```
