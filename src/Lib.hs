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
  where f_ = Memo.memo2 Memo.integral Memo.integral f'
          where
            f' 0 0 = (0, [])
            f' 0 j = (- j * d, [])
            f' i 0 = (- i * d, [])
            f' i j = let (fd, ad) = f_ (i-1) (j-1)
                         (fl, al) = f_ (i-1) j
                         (fu, au) = f_ i (j-1)
                         (a, b) = (x !! (i-1), y !! (j-1))
                         better (fx, _) (fy, _) = compare fx fy
                     in maximumBy better
                        [ (fd + score (x !! (i-1)) (y !! (j-1)) t, (a, b):ad)
                        , (fl - d, (a, "_"):al)
                        , (fu - d, ("_", b):au)]

outter :: [a] -> [a] -> [(a, a)]
outter xs ys = concat $ map (\y -> zip (repeat y) xs) ys

someFunc :: IO ()
someFunc = do
  fc <- readFile "BLOSUM62.txt"
  let (cs):rs = map words $ lines fc
      tab = map (match cs) rs
      x = (tolist "DEADLY")
      y = (tolist "DDGEARLYK")
      (s, alignment) = f 4 y x tab (length y) (length x)
  print s
  print . unzip . reverse $ alignment
  let (_, try) = f 4 (tolist hbb_human) (tolist insl3_human) tab (length hbb_human) (length insl3_human)
      (a, b) = unzip . reverse $ try
  print $ concat a
  print $ concat b
  -- let res = map (\(a, b) -> f 4 (tolist a) (tolist b) tab (length a) (length b)) pairs
  -- print res
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

pairs :: [(String, String)]
pairs = zip (repeat hbb_human) [ hbb_pantr
                               , hbb1_mouse
                               , hbb_chick
                               , q802a3_takru
                               , q540f0_vigun
                               , insl3_human]

hbb_human = "MVHLTPEEKSAVTALWGKVNVDEVGGEALGRLLVVYPWTQRFFESFGDLSTPDAVMGNPKVKAHGKKVLGAFSDGLAHLDNLKGTFATLSELHCDKLHVDPENFRLLGNVLVCVLAHHFGKEFTPPVQAAYQKVVAGVANALAHKYH"
hbb_pantr = "MVHLTPEEKSAVTALWGKVNVDEVGGEALGRLLVVYPWTQRFFESFGDLSTPDAVMGNPKVKAHGKKVLGAFSDGLAHLDNLKGTFATLSELHCDKLHVDPENFRLLGNVLVCVLAHHFGKEFTPPVQAAYQKVVAGVANALAHKYH"
hbb1_mouse = "MVHLTDAEKAAVSCLWGKVNSDEVGGEALGRLLVVYPWTQRYFDSFGDLSSASAIMGNAKVKAHGKKVITAFNDGLNHLDSLKGTFASLSELHCDKLHVDPENFRLLGNMIVIVLGHHLGKDFTPAAQAAFQKVVAGVATALAHKYH"
hbb_chick = "MVHWTAEEKQLITGLWGKVNVAECGAEALARLLIVYPWTQRFFASFGNLSSPTAILGNPMVRAHGKKVLTSFGDAVKNLDNIKNTFSQLSELHCDKLHVDPENFRLLGDILIIVLAAHFSKDFTPECQAAWQKLVRVVAHALARKYH"
q802a3_takru = "MVEWTDQERTIISNIFSTLDYEDVGSKSLIRCLIVYPWTQRYFAGFGNLYNAEAIKNNPNIAKHGVTVLHGLDRAVKNMDNIKETYKELSELHSEKLHVDPDNFKLLSDCLTIVVATKMGSKFTPEIQATFQKFLAVVVSALGRQYH"
q540f0_vigun = "MVAFSDKQEGLVNGAYEAFKADIPKYSVVFYTTILEKAPAAKNLFSFLANGVDATNPKLTGHAEKLFGLVRDSAAQLRASGGVVADAALGAVHSQKAVNDAQFVVVKEALVKTLKEAVGDKWSDELGTAVELAYDELAAAIKKAY"
insl3_human = "MDPRLPAWALVLLGPALVFALGPAPTPEMREKLCGHHFVRALVRVCGGPRWSTEARRPATGGDRELLQWLERRHLLHGLVADSNLTLGPGLQPLPQTSHHHRHHRAAATNPARYCCLSGCTQQDLLTLCPY"
