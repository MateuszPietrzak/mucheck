module Examples.GeneratedLemmaTest where

import Numeric.Natural (Natural)

insert :: Natural -> [Natural] -> [Natural]
insert x [] = [x]
insert x (y : ys)
  = if x <= y then x : (y : ys) else y : insert x ys

sort :: [Natural] -> [Natural]
sort [] = []
sort (x : xs) = insert x (sort xs)

isSorted :: [Natural] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x : (y : xs)) = x <= y && isSorted (y : xs)

iDecSorted :: [Natural] -> Bool
iDecSorted [] = True
iDecSorted [x] = True
iDecSorted (x : (y : xs)) = x <= y && iDecSorted (y : xs)

prop_sortIsSortedLemma :: [Natural] -> Bool
prop_sortIsSortedLemma xs = isSorted (sort xs) == True
{-# ANN prop_sortIsSortedLemma "Test sort" #-}

prop_sortLengthLemma :: [Natural] -> Bool
prop_sortLengthLemma xs = length (sort xs) == length xs
{-# ANN prop_sortLengthLemma "Test sort" #-}

prop_sortSortedLemma :: [Natural] -> Bool
prop_sortSortedLemma xs = iDecSorted (sort xs)
{-# ANN prop_sortSortedLemma "Test sort" #-}

prop_sortCorrectLemma :: [Natural] -> Bool
prop_sortCorrectLemma xs
  = isSorted (sort xs) == True && length (sort xs) == length xs
{-# ANN prop_sortCorrectLemma "Test sort" #-}

prop_sortIdempotentLemma :: [Natural] -> Bool
prop_sortIdempotentLemma xs = sort (sort xs) == sort xs
{-# ANN prop_sortIdempotentLemma "Test sort" #-}

prop_sortIsSortedIsTrue :: [Natural] -> Bool
prop_sortIsSortedIsTrue xs = isSorted (sort xs)
{-# ANN prop_sortIsSortedIsTrue "Test sort" #-}