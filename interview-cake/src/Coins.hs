module Coins where

import           Control.Monad.State
import           Data.Foldable       (foldl')
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import           Debug.Trace

type Denom = Int
type Amount = Int

-- Double Counting / Redundant Work
-- Outputs 7
denoms1 :: Amount -> [Denom] -> Int
denoms1 goal denoms = findDenoms1 goal denoms 0

findDenoms1 :: Amount -> [Denom] -> Amount -> Int
findDenoms1 goal denoms cur
  | goal <= 0 || cur > goal = 0
  | cur == goal = 1
  | otherwise = sum $ map (findDenoms1 goal denoms . (+ cur)) denoms

findDenoms2 :: S.Set Denom -> Amount -> Int
findDenoms2 denoms rem
  | rem == 0 = 1
  | rem < 0 = 0
  | S.null denoms = 0
  | otherwise = trace ("Finding ways to get " ++ show rem ++ " with " ++ show denoms) $
    sum $
      S.map
      (\coin ->
        findDenoms2 (coin `S.delete` denoms) rem +
        findDenoms2 denoms (rem - coin))
      denoms

findDenoms3 :: M.Map ([Denom], Amount) Int ->  ([Denom], Amount) -> Int
findDenoms3 prevSeen request = foldl' computeNumWays prevSeen (numWays request) M.! request

computeNumWays :: M.Map ([Denom], Amount) Int
                  -> ([Denom], Amount)
                  -> M.Map ([Denom], Amount) Int
computeNumWays  seen (denom, 0)    =  M.insert (denom,0) 1 seen
computeNumWays  seen ([], amount)  =  M.insert ([],amount) 0 seen
computeNumWays  seen key@(denom,amount) =
  let waysWithout =  memoLookup (init denom, amount) seen
      waysWith =  memoLookup (denom, amount - last denom) seen
  in  M.insert key (waysWithout + waysWith) seen

memoLookup :: ([Denom], Amount) -> M.Map ([Denom], Amount) Int  -> Int
memoLookup key@(_, amount) seen
  | amount < 0 = 0
  | key `M.member` seen = seen M.! key
  | otherwise = error $ "Key not found: " ++ show key

numWays :: ([Denom], Amount) -> [([Denom], Amount)]
numWays (denoms, amounts) = do
  subDenoms <- [[take n denoms] | n <- [0..length denoms]]
  subDenom <- subDenoms
  amount <- [0..amounts]
  pure (subDenom, amount)
