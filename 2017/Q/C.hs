import Data.Int

type I = Int64

solve :: (I,I) -> (I,I)
solve (n,k) = go k (n,1,0)
  where
    go k (n,a,b)
      | k <= a = g n
      | k <= a+b = g (n-1)
      | otherwise = go (k-a-b) $
        case r of
          0 -> (m,a,a+b*2)
          1 -> (m,2*a+b,b)
      where
        g n = (m,m+r-1) where (m,r) = divMod n 2
        (m,r) = divMod n 2

printCase (x,yz) = putStrLn $ concat $ ["Case #",show x,": ",p yz]
  where p (y,z) = unwords $ map show [y,z]

main = mapM_ printCase . zip [1..] . map f . g . lines =<< getContents
  where g (l:ls) = take t ls where t = read l
        f = solve . p . map read . take 2 . words
        p [a,b] = (a,b)
