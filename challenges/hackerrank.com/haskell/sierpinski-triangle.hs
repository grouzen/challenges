--
-- Solution for
--   https://www.hackerrank.com/challenges/functions-and-fractals-sierpinski-triangles
--

module Main where

import Data.Array.IO

type Vertice  = (Int, Int)
type Height   = Int

data Triangle = Tri Height Vertice (Maybe (Triangle, Triangle, Triangle))
              deriving (Show)

gen :: Int -> Height -> Vertice -> Triangle
gen 0 h p = Tri h p Nothing
gen n h p =
  let h'  = h `div` 2
      n'  = n - 1
      tt  = gen n' h' p
      tl  = gen n' h' ((fst p) - h', (snd p) + h')
      tr  = gen n' h' ((fst p) + h', (snd p) + h')
  in
   Tri h p $ Just (tt, tl, tr)

main :: IO ()
main = do
  let h = 32
      w = 63
      ts = gen 1 h ((w `div` 2) + 1, 0)
  canvas <- newArray (0, h * w) '_' :: IO (IOArray Int Char)

  printTriangles ts w canvas

  where
    printTriangles :: Triangle -> Int -> IO ()
    printTriangles t w c = do
      writeArray canvas 3 'a'
      printCanvas w canvas
      
    printCanvas :: Int -> IOArray Int Char -> IO ()
    printCanvas w c = do
      printLines 1 w c
      
      where
        printLine line w c = printLine' 0 line w c
          where
            printLine' pos line w c = do
              if pos >= w
                then putStr "\n"
                else do
                   p <- readArray c $ pos + (line * w)
                   putChar p
                   printLine' (pos + 1) line w c
                   
        printLines line w c = do
          (_, b) <- getBounds c
          if line * w >= b
            then return ()
            else do
               printLine line w c
               printLines (line + 1) w c
