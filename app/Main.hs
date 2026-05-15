module Main where
import System.Environment (getArgs)

import Test.MuCheck
import Test.MuCheck.TestAdapter.QuickCheckAdapter
import Test.MuCheck.TestAdapter
import Test.MuCheck.Utils.Print

main :: IO ()
main = do
  val <- getArgs
  case val of
    ("-h" : _ ) -> help
    ("-tix" : tix: file: _ ) -> do (msum, _tsum) <- mucheck (toRun file :: QuickCheckRun) tix
                                   print msum
    (file : _args) -> mucheck' (toRun file :: QuickCheckRun)
    _ -> error "Need function file [args]\n\tUse -h to get help"

help :: IO ()
help = putStrLn $ "mucheck function file [args]\n" ++ showAS ["E.g:",
       " mucheck [-tix <file.tix>] Examples/AssertCheckTest.hs",""]
