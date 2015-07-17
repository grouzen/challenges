
module Main where

f :: [Int] -> [Int]
f lst = filter' 1 lst
  where
    filter' :: Int -> [Int] -> [Int]
    filter' n []     = []
    filter' n (x:xs) | odd n     = filter' (n + 1) xs
                     | otherwise = x : filter' (n + 1) xs

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata
