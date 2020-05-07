module BinaryChop where

import           Control.Monad
import           Data.Bool

-- Take a Int to search for in a list.
-- Returns the index in the list or -1 if not found
type Index = Int

recursiveChop :: Int -> [Int] -> Index
recursiveChop n xs =
  let doChop :: Int -> [(Int,Int)] -> Index
      doChop _ [] = (-1)
      doChop n [(y, index)] = case compare n y of
        EQ -> index
        _  -> -1
      doChop n xs =
        let (lt,gt) = splitAt (length xs `div` 2) xs
            (pivot, index) = head gt
        in case compare n pivot of
          GT -> doChop n gt
          LT -> doChop n lt
          EQ -> index
  in doChop n (zip xs [0..])

functionalChop :: Int -> [Int] -> Index
functionalChop _ [] = (-1)
functionalChop n xs = let [(n', idx)] = until ((== 1) . length) (\xs ->
                                            let (lt,gt) = splitAt (length xs `div` 2) xs
                                                (pivot, index) = head gt
                                            in case compare n pivot of
                                              GT -> gt
                                              LT -> lt
                                              EQ -> [(n,index)])
                                          (zip xs [0..])
                     in bool (-1) idx (n == n')

-- arrayChop :: Int -> [Int] -> Index
arrayChop n xs =
  let pivots = tail . reverse $
        until ((==) 0 . head)
              (\p@(h:_) -> (h `div` 2):p)
              [length xs]
  in foldr (\curPivot retPivot -> bool retPivot curPivot (xs !! curPivot == n))
           (-1)
           pivots


assert_equal :: (Index, Index) -> IO ()
assert_equal (x,y) = when (x /= y) $ print (show x ++ show y)

testChop :: (Int -> [Int] -> Index) -> IO ()
testChop chop = do
  assert_equal((-1), chop 3 [])
  print "(-1), chop 3 [], PASS"
  assert_equal((-1), chop 3 [1])
  print "(-1), chop 3 [1], PASS"
  assert_equal(0,  chop 1 [1])
  print "0,  chop 1 [1], PASS"
  assert_equal(0,  chop 1 [1, 3, 5])
  print "0,  chop 1 [1, 3, 5], PASS"
  assert_equal(1,  chop 3 [1, 3, 5])
  print "1,  chop 3 [1, 3, 5], PASS"
  assert_equal(2,  chop 5 [1, 3, 5])
  print "2,  chop 5 [1, 3, 5], PASS"
  assert_equal((-1), chop 0 [1, 3, 5])
  print "(-1), chop 0 [1, 3, 5], PASS"
  assert_equal((-1), chop 2 [1, 3, 5])
  print "(-1), chop 2 [1, 3, 5], PASS"
  assert_equal((-1), chop 4 [1, 3, 5])
  print "(-1), chop 4 [1, 3, 5], PASS"
  assert_equal((-1), chop 6 [1, 3, 5])
  print "(-1), chop 6 [1, 3, 5], PASS"
  assert_equal(0,  chop 1 [1, 3, 5, 7])
  print "0,  chop 1 [1, 3, 5, 7], PASS"
  assert_equal(1,  chop 3 [1, 3, 5, 7])
  print "1,  chop 3 [1, 3, 5, 7], PASS"
  assert_equal(2,  chop 5 [1, 3, 5, 7])
  print "2,  chop 5 [1, 3, 5, 7], PASS"
  assert_equal(3,  chop 7 [1, 3, 5, 7])
  print "3,  chop 7 [1, 3, 5, 7], PASS"
  assert_equal((-1), chop 0 [1, 3, 5, 7])
  print "(-1), chop 0 [1, 3, 5, 7], PASS"
  assert_equal((-1), chop 2 [1, 3, 5, 7])
  print "(-1), chop 2 [1, 3, 5, 7], PASS"
  assert_equal((-1), chop 4 [1, 3, 5, 7])
  print "(-1), chop 4 [1, 3, 5, 7], PASS"
  assert_equal((-1), chop 6 [1, 3, 5, 7])
  print "(-1), chop 6 [1, 3, 5, 7], PASS"
  assert_equal((-1), chop 8 [1, 3, 5, 7])
  print "(-1), chop 8 [1, 3, 5, 7], PASS"
  print "Cases Complete"
