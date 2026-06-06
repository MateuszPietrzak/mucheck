module Examples.LambdaSubstTest where

import Test.QuickCheck

data Tm = Var Int
        | App Tm Tm
        | Lam Tm
  deriving (Eq, Show)

liftRen :: (Int -> Int) -> Int -> Int
liftRen _ 0 = 0
liftRen r n = r (n - 1) + 1

ren :: Tm -> (Int -> Int) -> Tm
ren (Var n)   r = Var (r n)
ren (App t u) r = App (ren t r) (ren u r)
ren (Lam t)   r = Lam (ren t (liftRen r))

liftSub :: (Int -> Tm) -> Int -> Tm
liftSub _ 0 = Var 0
liftSub s n = ren (s (n - 1)) (+ 1)

sub :: Tm -> (Int -> Tm) -> Tm
sub (Var n)   s = s n
sub (App t u) s = App (sub t s) (sub u s)
sub (Lam t)   s = Lam (sub t (liftSub s))

applyBeta :: Tm -> Tm -> Tm
applyBeta body arg = sub body s
  where
    s 0 = arg
    s k = Var (k - 1)

beta :: Tm -> Tm
beta (App (Lam body) arg) = applyBeta body arg
beta t = t

instance Arbitrary Tm where
  arbitrary = sized gen
    where
      gen n
        | n <= 0    = Var . getNonNegative <$> arbitrary
        | otherwise = frequency
            [ (1, Var . getNonNegative <$> arbitrary)
            , (2, App <$> gen (n `div` 2) <*> gen (n `div` 2))
            , (2, Lam <$> gen (n - 1))
            ]
  shrink (Var _)   = []
  shrink (App t u) = [t, u]
                  ++ [App t' u | t' <- shrink t]
                  ++ [App t u' | u' <- shrink u]
  shrink (Lam t)   = t : [Lam t' | t' <- shrink t]

prop_renId :: Tm -> Bool
prop_renId t = ren t id == t
{-# ANN prop_renId "Test ren" #-}

prop_subId :: Tm -> Bool
prop_subId t = sub t Var == t
{-# ANN prop_subId "Test sub" #-}

toRen :: Fun Int (NonNegative Int) -> (Int -> Int)
toRen (Fn f) = getNonNegative . f

prop_renComp :: Tm -> Fun Int (NonNegative Int) -> Fun Int (NonNegative Int) -> Bool
prop_renComp t f1 f2 = ren (ren t r1) r2 == ren t (r2 . r1)
  where r1 = toRen f1
        r2 = toRen f2
{-# ANN prop_renComp "Test ren" #-}

prop_renAsSub :: Tm -> Fun Int (NonNegative Int) -> Bool
prop_renAsSub t f = ren t r == sub t (Var . r)
  where r = toRen f
{-# ANN prop_renAsSub "Test ren" #-}

prop_subComp :: Tm -> Fun Int Tm -> Fun Int Tm -> Bool
prop_subComp t (Fn s1) (Fn s2) =
  sub (sub t s1) s2 == sub t (\n -> sub (s1 n) s2)
{-# ANN prop_subComp "Test sub" #-}

prop_subRenFusion :: Tm -> Fun Int (NonNegative Int) -> Fun Int Tm -> Bool
prop_subRenFusion t f (Fn s) = sub (ren t r) s == sub t (s . r)
  where r = toRen f
{-# ANN prop_subRenFusion "Test sub" #-}

prop_renSubFusion :: Tm -> Fun Int Tm -> Fun Int (NonNegative Int) -> Bool
prop_renSubFusion t (Fn s) f = ren (sub t s) r == sub t (\n -> ren (s n) r)
  where r = toRen f
{-# ANN prop_renSubFusion "Test sub" #-}
