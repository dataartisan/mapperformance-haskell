-- third shot at a solution for this assignment
-- http://distributed.etl.luc.edu/homework/assignment-1
-- to compile: ghc --make map.hs
-- laufer AT cs DOT luc DOT edu
-- Sat 19-Feb-2011

import Control.Monad
import Data.Char (ord)
import Data.List
import Data.Map hiding (map, split)
import Data.Time.Clock
import Maybe
import System (getArgs)
import System.Exit (exitFailure)
import System.Random
import System.Random.Shuffle

charRange = ('\000', '\127')
h = ord (snd charRange) - ord (fst charRange)

checkArgs :: (Int, Int, Int, Int) -> IO ()
checkArgs (n, d, k1, k2) = do
  when (d < 0 || 100 < d) $ do
    print "invalid percentage d"
    exitFailure
  when (2 * toInteger n > toInteger h ^ toInteger (k2 - k1)) $ do
    print "insufficient key range"
    exitFailure

randomChars gen = randomRs charRange gen
randomKeySizes (k1, k2) gen = randomRs (k1, k2) gen

randomKeysS :: (Int, Int) -> StdGen -> [String]
randomKeysS (k1, k2) gen =
  let
    (gen1, gen2) = split gen
  in
    spliterate (randomKeySizes (k1, k2) gen1) $ randomChars gen2

-- successively bite off chunks from cGen of sizes supplied by sGen
-- cannot be a fold because it consumes two generators
-- -> use scanl, which works on infinite lists
spliterate :: [Int] -> [a] -> [[a]]
spliterate sGen cGen =
  map fst $ drop 1 $ scanl (\(_, r) s -> splitAt s r) ([], cGen) sGen
{-
spliterate lGen cGen =
  let
    len : lRest = lGen
    (key, cRest) = splitAt len cGen
  in
    key : spliterate lRest cRest
-}

-- grow map m until it reaches size n from items supplied by src
-- cannot be a fold because termination is based on result size
-- -> use scanl for producing successive values and find to pick
-- the first one that satisfies our predicate
growMap :: Ord a => Int -> [a] -> Map a a -> Map a a
growMap n src m =
  fromJust $ find ((>= n) . size) $
    scanl (\m' key -> Data.Map.insert key key m') m src
{-
-- recursive version for comparison
growMap n src m | size m >= n = m
                | otherwise =
                  let
                    key : src' = src
                    m' = Data.Map.insert key key m
                  in
                    growMap n src' m'
-}

mkMap :: Ord a => Int -> [a] -> Map a a
mkMap n src = growMap n src empty

-- shrink map m by given percentage
shrinkMap :: Ord a => Int -> StdGen -> Map a a -> Map a a
shrinkMap perc gen m =
  let
    n = size m
    n1 = perc * n `div` 100
    ks = take n1 $ shuffle' (keys m) n gen
  in
    deleteKeys ks m

-- shrink map m by deleting the given list of keys
-- foldl' (strict) is 5x as fast as foldr and 2x as fast as foldl
deleteKeys :: Ord a => [a] -> Map a a -> Map a a
deleteKeys ks m = foldl' (flip Data.Map.delete) m ks
{-
-- recursive version for comparison
deleteKeys [] m =
deleteKeys (key : src) m =
  let
    m' = Data.Map.delete key m
  in
    deleteKeys src m'
-}

takeTime :: String -> (() -> Map a a) -> IO (Map a a)
takeTime msg task = do
  print msg
  time1 <- getCurrentTime
  let result = task ()
  print $ show $ size result
  time2 <- getCurrentTime
  print $ show $ diffUTCTime time2 time1
  return result

main :: IO ()
main = do
  args <- getArgs
  let [n, d, k1, k2] = map (read :: String -> Int) args
  checkArgs (n, d, k1, k2)
  print $ show [n, d, k1, k2]
  
  time1 <- getCurrentTime
  map1 <- takeTime "creating"  (\_ -> mkMap n $ randomKeysS (k1, k2) $ mkStdGen 1)
  map2 <- takeTime "shrinking" (\_ -> shrinkMap d (mkStdGen 2) map1)
  map3 <- takeTime "regrowing" (\_ -> growMap n (randomKeysS (k1, k2) $ mkStdGen 3) map2)
  time2 <- getCurrentTime
  print "total time"
  print $ show $ diffUTCTime time2 time1
