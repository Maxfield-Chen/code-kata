{-# LANGUAGE LambdaCase #-}

module Coins where

import           Control.Monad.State
import           Data.Foldable       (foldl')
import qualified Data.Map.Strict     as M
import           Debug.Trace

type Denom = Int
type Amount = Int
type NumWays = Int

-- Double Counting / Redundant Work
-- Outputs 7
denoms1 :: Amount -> [Denom] -> NumWays
denoms1 goal denoms = findDenoms1 goal denoms 0

findDenoms1 :: Amount -> [Denom] -> Amount -> NumWays
findDenoms1 goal denoms cur
  | goal <= 0 || cur > goal = 0
  | cur == goal = 1
  | otherwise = sum $ map (findDenoms1 goal denoms . (+ cur)) denoms

  -- Top Down Approach
makeChangeTopDown :: [ Denom ] -> Amount -> NumWays
makeChangeTopDown denoms amount
  | amount == 0 = 1
  | amount < 0 = 0
  | null denoms = 0
  | otherwise = trace ("Finding ways to get " ++ show amount ++ " with " ++ show denoms) $
        makeChangeTopDown (init denoms) amount +
        makeChangeTopDown denoms (amount - last denoms)

makeChangeBottomUp :: [ Denom ] -> Amount -> NumWays
makeChangeBottomUp _ 0  = 1
makeChangeBottomUp [] _ = 0
makeChangeBottomUp denoms amount =
  let denomsList = [take n denoms | n <- [1..length denoms]]
      amountsList = [0..amount]
      nextRow :: [Denom] -> [NumWays] -> [NumWays]
      nextRow denoms prevRow =
          let waysWithout = prevRow
              waysWith = replicate (last denoms) 0 ++ thisRow
              thisRow = zipWith3 (const (+)) amountsList waysWithout waysWith
          in thisRow
      firstRow = 1:map (const 0) amountsList
      changeTable = firstRow:zipWith nextRow denomsList changeTable
  in last $ last changeTable

  -- Bottom Up Approach w/ Memo
  -- Remember to Include LAMBDACASE
-- type CountedChange = M.Map ([Denom], Amount) NumWays

-- findInfiniteDenoms :: [Denom]  -> [NumWays]
-- findInfiniteDenoms denoms = iterate $ computeNumWays

-- computeNumWays :: ([Denom], Amount) -> State CountedChange NumWays
-- computeNumWays (denoms, 0) =
--   get >>=
--     put . M.insert (denoms,0) 1 >>
--     pure 1
-- computeNumWays ([], amount) =
--   get >>=
--     put . M.insert ([],amount) 0 >>
--     pure 0
-- computeNumWays key@(denoms,amount) =
--   do
--     seen <- get
--     let waysWithout =  memoLookup (init denoms, amount) seen
--         waysWith =  memoLookup (denoms, amount - last denoms) seen
--         newWays = waysWithout + waysWith
--     put $ M.insert key newWays seen
--     pure newWays

-- memoLookup :: ([Denom], Amount)
--            -> CountedChange
--            -> NumWays
-- memoLookup key@(denoms, amount) seen
--   | amount < 0 = 0
--   | key `M.member` seen = seen M.! key
--   | otherwise = error $ "Key not found: " ++ show key
