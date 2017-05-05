import Text.Printf
import Data.Monoid
import Data.List
import Data.Ord

type I = Integer

type Case = (Int, [(I,I)]) -- K [(Ri,Hi)]

data A = A {circle::I, cylinder::I, area::I} deriving Show

calc (r,h) = A a b (a+b) where a = r^2; b = 2*r*h

syrup xs = circle x + sum (map cylinder xs)
  where x = maximumBy (comparing circle) xs

solve :: Case -> I
solve (k,xs) = syrup $ g $ splitAt k $ sortBy cmp $ map calc xs
  where
    cmp = flip (comparing cylinder) <> comparing circle
    g (ys,zs) = if null ws then ys else w:init ys
      where
        w = maximumBy (comparing area) ws
        ws = filter (\x -> circle x >= m && area x > a) zs
          where
            a = m + cylinder (last ys)
            m = maximum $ map circle ys

parse [] = []
parse (x:xs) = (read k, map g ys) : parse zs
  where
    [n,k] = take 2 $ words x
    (ys,zs) = splitAt (read n) xs
    g = pair . map read . take 2 . words
    pair [a,b] = (a,b)

showCase (i,x) = concat ["Case #",show i,": ", printf "%f" a]
  where a = pi * (fromIntegral x :: Double)

byLines f = interact $ unlines . f . lines

main = byLines $ map showCase . zip [1..] . map solve . g
  where g (l:ls) = take (read l) (parse ls)
