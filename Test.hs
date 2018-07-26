

module Main where



import Data.List
import Data.Char
import Data.Ord
import Data.Kind
import System.IO
import System.Random
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Applicative ((<$>), (<|>))
import Criterion.Main

import qualified Data.Complex as DC
import qualified Digimon as Dg


--{-# LANGUAGE EmptyDataDecls, DataOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -ddump-ds #-}





--Tree Usage--
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

setTree :: Ord a => a -> Tree a -> Tree a
setTree a EmptyTree = Node a EmptyTree EmptyTree
setTree a (Node n left right) | a == n = Node a left right
                              | a < n  = Node n (setTree a left) right
                              | a > n  = Node n left (setTree a right)


----------------------------------------------------------------------------------------------


msg = (chr.(+24).ord) <$> "0IXXa\bJQZ\\PLIa"


randStd :: Int -> IO [Double]
randStd 0 = do return []
randStd n = do
  gen <- getStdGen
  let rand = fst (randomR (0, 1) gen :: (Double, StdGen))
  newStdGen
  next <- randStd (n-1)
  return $ rand : next




-- Sorting algorithms experiments

sampleGen :: Int -> IO [Double]
sampleGen r = do
  tmp <- randStd r
  return $ (*1000) <$> tmp



selectSort :: Ord a => [a] -> [a]
selectSort [] = []
selectSort l = mini : (selectSort $ delete mini l)
  where mini = minimum l

insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x $ insertSort xs
  where insert t hub = case hub of
          [] -> [t]
          (x:xs) | t < x     -> t:x:xs
                 | otherwise -> x : (insert t xs)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
  where smaller = [y | y <- xs, y <= x]
        larger  = [y | y <- xs, y > x]



------------------Haskell 99 Questions--------------------



--Q1
myLast :: [a] -> a
myLast [] = error "Empty list!"
myLast (x:xs) = case xs of
  [] -> x
  _  -> myLast xs

--Q2
myButLast :: [a] -> a
myButLast [] = error "Empty list!"
myButLast (x:xs) = case xs of
  []     -> error "No but last elem!"
  (y:ys) -> case ys of
    [] -> x
    _  -> myButLast xs

myButLast' :: [a] -> a
myButLast' = last . init

--Q3
elemAt :: [a] -> Int -> a
elemAt [] _ = error "Iter is too big!"
elemAt (x:xs) i = case i of
  0 -> x
  _ -> elemAt xs $ i - 1

--Q4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--Q5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

--Q6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

--Q7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten $ List xs)

--Q8
compress :: Eq a => [a] -> [a]
compress = reverse . foldl (\acc x -> case acc of
  []     -> [x]
  (y:ys) -> if x == y then acc else x:acc) []

--Q9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, rest) = span (==x) xs
  in (x : first) : pack rest

--Q10
encode :: Eq a => [a] -> [(Int, a)]
encode l = (\(x:xs) -> (length xs + 1, x)) <$> (pack l)

--Q11
data ListItem a = Single a | Multiple Int a 
  deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified l = (\(n, c) -> case n of
  1 -> Single c
  _ -> Multiple n c) <$> (encode l)

--Q12
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified l = l >>= (\e -> case e of
  Single c     -> [c]
  Multiple n c -> replicate n c)

--Q13

--Q14
dupli :: Eq a => [a] -> [a]
dupli l = (encode l) >>= (\(n, c) -> replicate (n * 2) c)

--Q15
repli :: Eq a => [a] -> Int -> [a]
repli l n = l >>= (\e -> replicate n e)

--Q16
dropEvery :: [a] -> Int -> [a]
dropEvery l n = undefined

--Q17*
split' :: [a] -> Int -> ([a], [a])
split' (x:xs) n | n > 0 = let (f,l) = split' xs (n-1) in (x : f, l)
split' xs _  = ([], xs)

--Q18
slice :: [a] -> Int -> Int -> Maybe [a]
slice [] s e = Just []
slice l@(x:xs) s e  | s == e = Just []
                    | s > e || s > length l || e > length l ||
                      s < 0 || e < 0 = Nothing
                    | s == 0 = Just $ take e xs
                    | otherwise = Just $ drop (s-1) $ take e xs

--Q19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate l@(x:xs) n = let 
  (first, rest) = split' l (if n >= 0 then n 
                            else (length l + n)) in rest ++ first

--Q20
removeAt :: Int -> [a] -> (a, [a])
removeAt i l = (l !! (i-1), (init x) ++ y) where
  (x, y) = split' l i

--Q21
insertAt :: a -> [a] -> Int -> [a]
insertAt e l i = f ++ [e] ++ s where
  (f, s) = split' l i

--Q22
range :: Int -> Int -> [Int]
range s e = [s .. e]

--Q23*
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect l n | n < 0 = error "range must >= 0"
              | otherwise = do 
                raw <- randStd n
                ids <- pure ((floor.(*(fromIntegral $ length l))) <$> raw)
                return [l !! i | i <- ids]


--Q24
diffSelect :: Int -> Int -> IO [Int]
diffSelect n r = rndSelect [1 .. r] n

--Q25*

--Q26*
combinations :: Eq a => Int -> [a] -> [[a]]
combinations n l = 
  if n <= 0 then return [] 
  else [y:ys | y:xs' <- tails l
             , ys <- combinations (n-1) xs']

--Q27*

--Q28

--(a
lsort :: [[a]] -> [[a]]
lsort = sortOn length

--(b
lfSort :: [[a]] -> [[a]]
lfSort l = sortOn (frequency l) l where
  frequency l e = 
    foldl (\acc x -> if length x == length e then acc + 1 else acc) 0 l



--Q29*  Here we need to learn more on hierarchy of Haskell API
isPrime :: Integral a => a -> Bool
isPrime n = 
  foldl (\acc x -> 
    if acc && n `mod` x /= 0 then True else False) True [2 .. end]
  where end = n - 1

--Q30
gcd' :: Int -> Int -> Int
gcd' a b = if b == 0 then abs a else gcd' b $ a `mod` b

--Q31
coPrime :: Int -> Int -> Bool
coPrime a b = gcd a b == 1

--Q32







sample = sampleGen 100
-- Our benchmark harness.
criterion = defaultMain [
  bgroup "Sorting" [ bench "system"     $ whnfIO (sort <$> sample)
                   , bench "selectSort" $ whnfIO (selectSort <$> sample)
                   , bench "insertSort" $ whnfIO (insertSort <$> sample)
                   , bench "quickSort"  $ whnfIO (quickSort <$> sample)
                   ]
  ]


main = criterion























