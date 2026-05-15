module Examples.QuickCheckTest where

import Test.QuickCheck
import Data.List (intercalate)

unsplit :: Char -> [String] -> String
unsplit c = intercalate [c]

split :: Char -> String -> [String]
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs

prop_splitInv xs
    = forAll (elements xs) $ \c ->
      unsplit c (split c xs) == xs
{-# ANN prop_splitInv "Test split" #-}


f1 :: Int -> Int -> Bool
f1 x y = x == y

f2 :: Int -> Int -> Bool
f2 x y = f1 x y

prop_CalculatesEq :: Int -> Int -> Bool
prop_CalculatesEq x y = (x == y) == f2 x y
{-# ANN prop_CalculatesEq "Test f2" #-}

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x : xs) = qsort l ++ [x] ++ qsort r
    where l = filter (< x) xs
          r = filter (>= x) xs

prop_LenPreserved :: [Int] -> Bool
prop_LenPreserved xs = length xs == (length . qsort) xs
{-# ANN prop_LenPreserved "Test qsort" #-}

prop_IsSorted :: [Int] -> Bool
prop_IsSorted = isSorted . qsort
  where
    isSorted [] = True
    isSorted [x] = True
    isSorted (x:y:xs) = x < y && isSorted (y:xs)
{-# ANN prop_IsSorted "Test qsort" #-}