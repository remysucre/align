module Lib where
import qualified Data.MemoCombinators as Memo
import Data.List
import System.Random.Shuffle
import Control.Monad
import Debug.Trace

type Tab = [(String, [(String, Int)])]

-- configurations

-- number of permutations for p values
mutants :: Int
mutants = 1000

-- gap penalty
gapd :: Int
gapd = 4

-- gap penalty for smith waterman
gapsw :: Int
gapsw = 8

-- NW algorithm: no tables needed, directly implement the recursion scheme
-- and dynamic programming comes for free with automatic memoization

f :: Int -> [String] -> [String] -> Tab -> Int -> Int -> (Int, [(String, String)])
f d x y t = trace ("aligning " ++ concat (take 10 y) ++ "...") f_
  where f_ = Memo.memo2 Memo.integral Memo.integral f'
          where
            -- initialization
            f' 0 0 = (0, [])
            f' 0 j = (- j * d, [])
            f' i 0 = (- i * d, [])
            -- inductive step
            f' i j = let (fd, ad) = f_ (i-1) (j-1) -- diagonal
                         (fl, al) = f_ (i-1) j     -- left
                         (fu, au) = f_ i (j-1)     -- right
                         (a, b) = (x !! (i-1), y !! (j-1)) -- 0-index
                         better (fx, _) (fy, _) = compare fx fy
            -- pick best
                     in maximumBy better
                        [ (fd + score (x !! (i-1)) (y !! (j-1)) t, (a, b):ad)
                        , (fl - d, (a, "_"):al)
                        , (fu - d, ("_", b):au)]

-- smith waterman version
fsw :: Int -> [String] -> [String] -> Tab -> Int -> Int -> ((Int, [(String, String)]), (Int, [(String, String)]))
fsw d x y t = trace ("aligning " ++ concat (take 10 y) ++ "...") f_
  where f_ = Memo.memo2 Memo.integral Memo.integral f'
          where
            f' 0 _ = ((0, []), (0, [])) -- second result is best so far
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

someFunc :: IO ()
someFunc = do
  -- read BLOSUM62 matrix
  fc <- readFile "BLOSUM62.txt"
  let cs:rs = map words $ lines fc
      tab = map (tally cs) rs
  -- aligning DEADLY and DDGEARLYK
  print "aligning DEADLY and DDGEARLYK"
  let x = tolist "DEADLY"
      y = tolist "DDGEARLYK"
      dead = f gapd y x tab (length y) (length x)
  print "alignment result"
  printAlignment dead
  deadp <- pval y x tab
  print "deadly p value"
  print deadp
  -- aligning proteins
  print "aligning proteins"
  let res = map (\(l, r) -> f gapd (tolist l) (tolist r) tab (length l) (length r)) pairs
  print "alignment results"
  _ <- mapM printAlignment res
  pvals <- mapM (\(l, r) -> pval (tolist l) (tolist r) tab) pairs
  print "protein p values"
  print pvals
  -- local alignment
  print "local alignment"
  let locx = tolist "SRGMIEVGNQWT"
      locy = tolist "RGMVVGRW"
      (_, ans) = fsw gapsw locx locy tab (length locx) (length locy)
  printAlignment ans
  print "DONE"

-- p value of sequence alignment

pval :: [String] -> [String] -> Tab -> IO Double
pval xs ys tab = do
  -- get permutations
  perms <- replicateM mutants $ shuffleM ys
  -- align permutations
  let (sperms, _) = unzip $ map (\p -> f gapd xs p tab (length xs) (length p)) perms
  -- align original
      (sal, _) = f gapd xs ys tab (length xs) (length ys)
  -- percentile of original alignment
  return $ fromIntegral (length (filter (> sal) sperms)) / fromIntegral (length sperms)

printAlignment :: (Int, [(String, String)]) -> IO()
printAlignment (s, a) = do
  print s
  let (x, y) = unzip . reverse $ a
  print $ concat x
  print $ concat y

tolist :: String -> [String]
tolist = map (: [])

-- read a score entry from the input matrix file

tally :: [String] -> [String] -> (String, [(String, Int)])
tally cs (a:ss) =
  let ns = map read ss :: [Int]
   in (a, zip cs ns)
tally _ _ = undefined

-- lookup a match from the scoring matrix

score :: String -> String -> [(String, [(String, Int)])] -> Int
score x y t =
  let Just r = lookup y t
      Just n = lookup x r
  in n

-- protein pairs to align

pairs :: [(String, String)]
pairs = zip (repeat hbb_human) [ hbb_pantr
                               , hbb1_mouse
                               , hbb_chick
                               , q802a3_takru
                               , q540f0_vigun
                               , insl3_human]

-- proteins

hbb_human = "MVHLTPEEKSAVTALWGKVNVDEVGGEALGRLLVVYPWTQRFFESFGDLSTPDAVMGNPKVKAHGKKVLGAFSDGLAHLDNLKGTFATLSELHCDKLHVDPENFRLLGNVLVCVLAHHFGKEFTPPVQAAYQKVVAGVANALAHKYH"
hbb_pantr = "MVHLTPEEKSAVTALWGKVNVDEVGGEALGRLLVVYPWTQRFFESFGDLSTPDAVMGNPKVKAHGKKVLGAFSDGLAHLDNLKGTFATLSELHCDKLHVDPENFRLLGNVLVCVLAHHFGKEFTPPVQAAYQKVVAGVANALAHKYH"
hbb1_mouse = "MVHLTDAEKAAVSCLWGKVNSDEVGGEALGRLLVVYPWTQRYFDSFGDLSSASAIMGNAKVKAHGKKVITAFNDGLNHLDSLKGTFASLSELHCDKLHVDPENFRLLGNMIVIVLGHHLGKDFTPAAQAAFQKVVAGVATALAHKYH"
hbb_chick = "MVHWTAEEKQLITGLWGKVNVAECGAEALARLLIVYPWTQRFFASFGNLSSPTAILGNPMVRAHGKKVLTSFGDAVKNLDNIKNTFSQLSELHCDKLHVDPENFRLLGDILIIVLAAHFSKDFTPECQAAWQKLVRVVAHALARKYH"
q802a3_takru = "MVEWTDQERTIISNIFSTLDYEDVGSKSLIRCLIVYPWTQRYFAGFGNLYNAEAIKNNPNIAKHGVTVLHGLDRAVKNMDNIKETYKELSELHSEKLHVDPDNFKLLSDCLTIVVATKMGSKFTPEIQATFQKFLAVVVSALGRQYH"
q540f0_vigun = "MVAFSDKQEGLVNGAYEAFKADIPKYSVVFYTTILEKAPAAKNLFSFLANGVDATNPKLTGHAEKLFGLVRDSAAQLRASGGVVADAALGAVHSQKAVNDAQFVVVKEALVKTLKEAVGDKWSDELGTAVELAYDELAAAIKKAY"
insl3_human = "MDPRLPAWALVLLGPALVFALGPAPTPEMREKLCGHHFVRALVRVCGGPRWSTEARRPATGGDRELLQWLERRHLLHGLVADSNLTLGPGLQPLPQTSHHHRHHRAAATNPARYCCLSGCTQQDLLTLCPY"
