module Main where
    
import Distribution.Simple
import System.Cmd(system)

main = defaultMainWithHooks $ simpleUserHooks { runTests = runTests_ }

runTests_ a b pd lb = system "runhaskell -i./src ./tests/Test.hs" >> return ()
