module Meetings where

type Meeting = (Int, Int)

converge :: (a -> a -> Bool) -> [a] -> a
converge predicate (x:ys@(y:_))
  | predicate x y = y
  | otherwise = converge predicate ys

mergeMeetings :: [Meeting] -> [Meeting]
mergeMeetings meetings = converge (==) $ iterate (foldr addMeeting []) (mergeSort meetings)

--Available is built and needs to be reversed at the end
addMeeting :: Meeting -> [Meeting] -> [Meeting]
addMeeting m [] = [m]
addMeeting (start, end) available@((aStart, aEnd):xs) =
  case aStart > end of
    True  -> (start,end):available
    False -> (min aStart start, max end aEnd):xs

mergeSort :: [Meeting] -> [Meeting]
mergeSort []     = []
mergeSort [a]    = [a]
mergeSort toSort =
  let half = length toSort `div` 2
      left = mergeSort $ take half toSort
      right = mergeSort $ drop half toSort
  in  mergeHalves left right

mergeHalves :: [Meeting] -> [Meeting] -> [Meeting]
mergeHalves x []          = x
mergeHalves [] y          = y
mergeHalves (x:xs) (y:ys) | fst y < fst x = y : mergeHalves (x:xs) ys
mergeHalves (x:xs) (y:ys) = x : mergeHalves xs (y:ys)

mergeSortTests :: IO ()
mergeSortTests = do
  print (mergeMeetings [(0,1), (3,5),(4,8),(9,10),(10,12)])
  print (mergeMeetings [(1, 2), (2, 3)])
  print (mergeMeetings [(1, 5), (2, 3)])
  --Didn't catch recursive case.
  print (mergeMeetings [(1, 10), (2, 6), (3, 5), (7, 9)])
