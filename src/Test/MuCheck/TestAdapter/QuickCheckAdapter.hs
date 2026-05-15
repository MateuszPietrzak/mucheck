{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
-- | Module for using quickcheck properties
module Test.MuCheck.TestAdapter.QuickCheckAdapter where
import Test.MuCheck.TestAdapter
import qualified Test.QuickCheck as QC

type QuickCheckSummary = QC.Result

instance Summarizable QuickCheckSummary where
  testSummary _mutant _test result = Summary $ _ioLog result
  isSuccess (result) = QC.isSuccess result
  isFailure = not . isSuccess
  isOther   _               = False


newtype QuickCheckRun = QuickCheckRun String

instance TRun QuickCheckRun QuickCheckSummary where
  genTest _m tstfn = "quickCheckWithResult stdArgs {chatty = False} " ++ tstfn
  getName (QuickCheckRun str) = str
  toRun s = QuickCheckRun s

  summarize_ _m = testSummary :: Mutant -> TestStr -> InterpreterOutput QuickCheckSummary -> Summary
  success_ _m = isSuccess :: QuickCheckSummary -> Bool
  failure_ _m = isFailure :: QuickCheckSummary -> Bool
  other_ _m = isOther :: QuickCheckSummary -> Bool

