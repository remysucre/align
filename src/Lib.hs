{-# LANGUAGE BangPatterns #-}

module Lib where
import qualified Data.MemoCombinators as Memo
import Data.List
import Debug.Trace

type Tab = [(String, [(String, Int)])]

fib :: Int -> Int
fib = Memo.integral fib'
    where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n-1) + fib (n-2)

f :: Int -> [String] -> [String] -> Tab -> Int -> Int -> (Int, [(String, String)])
f d x y t = f_
  where f_ = Memo.integral f'
          where
            f' 0 0 = (0, []) --[(x !! 0, y !! 0)])
            f' 0 j = (- j * d, []) --zip (repeat "_") (take j y))
            f' i 0 = (- i * d, []) --zip (take i x) (repeat "_"))
            f' i j = let (fd, ad) = f_ (i-1) (j-1)
                         (fl, al) = f_ (i-1) j
                         (fu, au) = f_ i (j-1)
                         (a, b) = (x !! (i-1), y !! (j-1))
                         better (fx, _) (fy, _) = compare fx fy
                     in maximumBy better
                        [ (fd + score (x !! (i-1)) (y !! (j-1)) t, (a, b):ad)
                        , (fl - d, (a, "_"):al)
                        , (fu - d, ("_", b):au)]

someFunc :: IO ()
someFunc = do
  fc <- readFile "BLOSUM62.txt"
  let (cs):rs = map words $ lines fc
      tab = map (match cs) rs
  print $ score "S" "T" tab
  print $ score "M" "D" tab
  let x = (tolist "DEADLY")
      y = (tolist "DDGEARLYK")
      (s, alignment) = f 4 y x tab (length y) (length x)
  print s
  print . unzip . reverse $ alignment
  print "DONE"

tolist :: String -> [String]
tolist cs = map (\c -> [c]) cs

match :: [String] -> [String] -> (String, [(String, Int)])
match cs (a:ss) =
  let ns = map read ss :: [Int]
   in (a, zip cs ns)
match _ _ = undefined

score :: String -> String -> [(String, [(String, Int)])] -> Int
score x y t =
  let Just r = lookup y t
      Just n = lookup x r
  in n
