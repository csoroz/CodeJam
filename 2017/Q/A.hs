{-# LANGUAGE BangPatterns #-}
import Data.List

solve :: Int -> String -> Maybe Int
solve k = go 0
  where
    go !n [] = Just n
    go !n ('+':xs) = go n xs
    go !n xs = next (splitAt k xs)
      where next (ys,zs) 
              | length ys == k = go (n+1) (map inv ys ++ zs)
              | otherwise = Nothing
              where inv '-' = '+'
                    inv '+' = '-'

parse :: String -> (Int,String)
parse = g . take 2 . words where g [xs,k] = (read k, xs)

showCase (i,x) = concat $ ["Case #",show i,": ",p x]
  where p Nothing = "IMPOSSIBLE"
        p (Just n) = show n

byLines f = interact $ unlines . f . lines

main = byLines $ map showCase . zip [1..] . map f . g
  where g (l:ls) = take t ls where t = read l
        f = uncurry solve . parse
