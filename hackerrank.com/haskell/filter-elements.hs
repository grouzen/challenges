
module Main where

runTestCase :: IO ()
runTestCase = do
  putStrLn "test"
  return ()


main :: IO ()
main = do
  tcnum <- readLn :: IO Int

  loop tcnum

  where
    loop :: Int -> IO ()
    loop 0 = return ()
    loop n = do { runTestCase; loop (n - 1); }
