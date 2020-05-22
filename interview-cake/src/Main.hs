
module Main where

import qualified Coins           as C
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Meetings        as Meet
import qualified MeshNetwork     as MeshNet

main :: IO ()
main = print $ C.makeChangeBottomUp [1,2] 3
-- main = print $ C.findDenoms3 mempty ([1,2], 3)
