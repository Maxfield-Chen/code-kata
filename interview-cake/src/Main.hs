
module Main where

import qualified Coins           as C
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Meetings        as Meet
import qualified MeshNetwork     as MeshNet

main :: IO ()
main = print $ C.findDenoms3 M.empty ([1,5,10], 12)
