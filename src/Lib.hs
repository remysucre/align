module Lib where
import qualified Data.MemoCombinators as Memo
import Data.List
import System.Random.Shuffle
import Control.Monad
import Debug.Trace

type Tab = [(String, [(String, Int)])]

mutants :: Int
mutants = 1000

gapd :: Int
gapd = 4

gapsw :: Int
gapsw = 8

f :: Int -> [String] -> [String] -> Tab -> Int -> Int -> (Int, [(String, String)])
f d x y t = trace ("aligning " ++ concat (take 10 y) ++ "...") f_
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

fsw :: Int -> [String] -> [String] -> Tab -> Int -> Int -> ((Int, [(String, String)]), (Int, [(String, String)]))
fsw d x y t = trace ("aligning " ++ concat (take 10 y) ++ "...") f_
  where f_ = Memo.memo2 Memo.integral Memo.integral f'
          where
            f' 0 _ = ((0, []), (0, []))
            f' _ 0 = ((0, []), (0, []))
            f' i j = let ((fd, ad), bestd) = f_ (i-1) (j-1)
                         ((fl, al), bestl) = f_ (i-1) j
                         ((fu, au), bestu) = f_ i (j-1)
                         (a, b) = (x !! (i-1), y !! (j-1))
                         better (fx, _) (fy, _) = compare fx fy
                         bests = [bestd, bestl, bestu]
                         ans = maximumBy better
                               [ (0, [])
                               , (fd + score (x !! (i-1)) (y !! (j-1)) t, (a, b):ad)
                               , (fl - d, (a, "_"):al)
                               , (fu - d, ("_", b):au)]
                         best = maximumBy better (ans : bests)
                     in (ans, best)

outter :: [a] -> [a] -> [(a, a)]
outter xs = concatMap (\y -> zip (repeat y) xs)

someFunc :: IO ()
someFunc = do
  fc <- readFile "BLOSUM62.txt"
  let cs:rs = map words $ lines fc
      tab = map (match cs) rs
      x = tolist "DEADLY"
      y = tolist "DDGEARLYK"
      dead = f gapd y x tab (length y) (length x)
  printAlignment dead
  deadp <- pval y x tab
  print deadp
  let (_, try) = f gapd (tolist hbb_human) (tolist insl3_human) tab (length hbb_human) (length insl3_human)
      (a, b) = unzip . reverse $ try
  print $ concat a
  print $ concat b
  let res = map (\(l, r) -> f gapd (tolist l) (tolist r) tab (length l) (length r)) pairs
  _ <- mapM printAlignment res
  pvals <- mapM (\(l, r) -> pval (tolist l) (tolist r) tab) pairs
  print pvals
  let locx = tolist "SRGMIEVGNQWT"
      locy = tolist "RGMVVGRW"
      (_, ans) = fsw gapsw locx locy tab (length locx) (length locy)
  printAlignment ans
  print "DONE"

pval :: [String] -> [String] -> Tab -> IO Double
pval xs ys tab = do
  perms <- replicateM mutants $ shuffleM ys
  let (sperms, _) = unzip $ map (\p -> f gapd xs p tab (length xs) (length p)) perms
      (sal, _) = f gapd xs ys tab (length xs) (length ys)
  return $ fromIntegral (length (filter (> sal) sperms)) / fromIntegral (length sperms)

printAlignment :: (Int, [(String, String)]) -> IO()
printAlignment (s, a) = do
  print s
  let (x, y) = unzip . reverse $ a
  print $ concat x
  print $ concat y

tolist :: String -> [String]
tolist = map (: [])

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
