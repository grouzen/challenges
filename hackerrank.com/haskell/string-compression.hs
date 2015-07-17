
module Main where

compress :: String -> String
compress = foldr (++) "" . map show' . split' []
  where
    show' :: String -> String
    show' str | length str == 1 = str
              | otherwise      = head str : (show $ length str)
                                 
    split' :: String -> String -> [String]
    split' acc []    = [acc]
    split' [] (x:xs) = split' [x] xs
    split' acc@(y:ys) str@(x:xs) | y == x     = split' (x:acc) xs
                                 | otherwise = acc : split' [] str
    
main = do
  str <- getLine
  putStrLn $ compress str
