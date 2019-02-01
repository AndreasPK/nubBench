module Main where

-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
-- import Data.Text (Text)

import Control.Monad
import Control.Monad.ST
import qualified Data.Set as S
import Data.Array.ST
import Data.Hashable
import Criterion.Main


import Data.List
import Data.Containers.ListUtils
import qualified Relude.Nub as Relude
import qualified Data.Discrimination as DD

main :: IO ()
main = do
    content <- words <$> readFile "Main.dump-stg"
    dubFree <- return . nubOrd $ content
    let sizes = [10,20,50,100,500]
    -- let sizes = [20000]
    defaultMain
      [ bgroup "dups"    [bgroup "strict" (map (mkBenchsStrictText content) sizes)]
      , bgroup "dubFree" [bgroup "strict" (map (mkBenchsStrictText content) sizes)]

      -- , bgroup "lazy-dubs" (map (mkBenchsLazyText content)   sizes)
      ]
    print $ length $ nubOrd $ content

mkBenchsStrictText :: [String] -> Int -> Benchmark
mkBenchsStrictText content size =
  let input = take size content
  in  bgroup (show size) [ bench "blog_Spence" $ whnf (length . nubSpence) input
      , bench "blog_gspia"  $ whnf (length . nubReddit) input
      , bench "nub" $ whnf (length . nub) input
      , bench "nubOrd" $ whnf (length . nubOrd) input
      -- , bench "rel_sort" $ whnf (length . Relude.sortNub) input
      , bench "rel_hash" $ whnf (length . Relude.hashNub) input
      , bench "rel_unstable" $ whnf (length . Relude.unstableNub) input
      , bench "dd_nub" $ whnf (length . DD.nub) input

      -- , bench "set_unstable" $ whnf (length . nubSet) input
      ]

mkBenchsLazyText :: [String] -> Int -> Benchmark
mkBenchsLazyText content size =
  let input = take size content
  in  bgroup (show size) [ bench "blog_Spence" $ whnf (nubSpence) input
      , bench "blog_gspia"  $ whnf (nubReddit) input
      , bench "nub" $ whnf (nub) input
      , bench "nubOrd" $ whnf (nubOrd) input
      -- , bench "rel_sort" $ whnf (Relude.sortNub) input
      , bench "rel_hash" $ whnf (Relude.hashNub) input
      , bench "rel_unstable" $ whnf (Relude.unstableNub) input
      , bench "dd_nub" $ whnf (DD.nub) input

      -- , bench "set_unstable" $ whnf (nubSet) input
      ]

nubSpence :: (Hashable a, Eq a) => [a] -> [a]
nubSpence l = runST $ do
  arr <- mr
  forM_ l $ \j -> do
    let index = (hash j) `mod` 255
    current <- readArray arr index
    let new = if j `elem` current then current else j : current
    writeArray arr index new
  join <$> getElems arr
    where
      mr :: ST s (STArray s Int [a])
      mr = newListArray (0, 255) (replicate 256 [])

nubReddit :: (Hashable a, Eq a) => [a] -> [a]
nubReddit l = runST $ do
  arr <- mr
  forM_ l $ \j -> do
    let index = (hash j) `mod` 255
    current <- readArray arr index
    if j `elem` current
                then pure ()
                else writeArray arr index (j : current)
  join <$> getElems arr
    where
      mr :: ST s (STArray s Int [a])
      mr = newListArray (0, 255) (replicate 256 [])

nubSet :: Ord a => [a] -> [a]
nubSet = S.toList . S.fromList