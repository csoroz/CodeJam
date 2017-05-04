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

printCase (i,x) = putStrLn $ concat $ ["Case #",show i,": ",p x]
  where p Nothing = "IMPOSSIBLE"
        p (Just n) = show n

main = mapM_ printCase . zip [1..] . map f . g . lines =<< getContents
  where f = uncurry solve . parse
        g (l:ls) = take t ls where t = read l
