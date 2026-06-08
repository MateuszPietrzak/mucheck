module Examples.GeneratedLambdaSubstTest where

import Test.QuickCheck

-- beginning of code generated with agda2hs

import Numeric.Natural (Natural)

data TmNat = TmZero
           | TmSuc TmNat
               deriving (Eq, Show)

tmToNat :: TmNat -> Natural
tmToNat TmZero = 0
tmToNat (TmSuc x) = 1 + tmToNat x

data Tm = Var TmNat
        | App Tm Tm
        | Lam Tm
            deriving (Eq, Show)

liftRen :: (TmNat -> TmNat) -> TmNat -> TmNat
liftRen _ TmZero = TmZero
liftRen r (TmSuc n) = TmSuc (r n)

ren :: Tm -> (TmNat -> TmNat) -> Tm
ren (Var n) r = Var (r n)
ren (App t u) r = App (ren t r) (ren u r)
ren (Lam t) r = Lam (ren t (liftRen r))

liftSub :: (TmNat -> Tm) -> TmNat -> Tm
liftSub _ TmZero = Var TmZero
liftSub s (TmSuc n) = ren (s n) TmSuc

sub :: Tm -> (TmNat -> Tm) -> Tm
sub (Var n) s = s n
sub (App t u) s = App (sub t s) (sub u s)
sub (Lam t) s = Lam (sub t (liftSub s))

fromNatAcc :: Natural -> TmNat
fromNatAcc n
  = if n == 0 then TmZero else TmSuc (fromNatAcc (n - 1))

tmFromNat :: Natural -> TmNat
tmFromNat n = fromNatAcc n

-- end of code generated with agda2hs

instance Arbitrary Tm where
  arbitrary = sized gen
    where
      gen n
        | n <= 0    = Var . tmFromNat . getNonNegative <$> arbitrary
        | otherwise = frequency
            [ (1, Var . tmFromNat . getNonNegative <$> arbitrary)
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

toRen :: Fun Int (NonNegative Int) -> (TmNat -> TmNat)
toRen (Fn f) = tmFromNat . fromIntegral . getNonNegative . f . fromIntegral . tmToNat

toSub :: Fun Int Tm -> (TmNat -> Tm)
toSub (Fn s) = s . fromIntegral . tmToNat

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
prop_subComp t f1 f2 =
  sub (sub t s1) s2 == sub t (\n -> sub (s1 n) s2)
  where s1 = toSub f1
        s2 = toSub f2
{-# ANN prop_subComp "Test sub" #-}

prop_subRenFusion :: Tm -> Fun Int (NonNegative Int) -> Fun Int Tm -> Bool
prop_subRenFusion t f g = sub (ren t r) s == sub t (s . r)
  where r = toRen f
        s = toSub g
{-# ANN prop_subRenFusion "Test sub" #-}

prop_renSubFusion :: Tm -> Fun Int Tm -> Fun Int (NonNegative Int) -> Bool
prop_renSubFusion t g f = ren (sub t s) r == sub t (\n -> ren (s n) r)
  where s = toSub g
        r = toRen f
{-# ANN prop_renSubFusion "Test sub" #-}
