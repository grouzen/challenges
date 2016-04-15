len :: [a] -> Int
len lst = (last . (:) 0 . map fst . zip [1..]) lst

