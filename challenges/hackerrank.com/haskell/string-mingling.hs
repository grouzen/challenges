
f :: String -> String -> String
f s1 s2 = [x | ab <- zip s1 s2, x <- fst ab : [snd ab] ]

main = do
  c <- getContents
  
  let lns    = lines c
      (a, b) = (lns !! 0, lns !! 1)
      
  putStrLn $ f a b
      
  
