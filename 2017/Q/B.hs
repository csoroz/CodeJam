
tidy :: String -> String
tidy (x:xs) = dropWhile (=='0') $ h $ g $ f ([x],xs)
  where
    h (xs,ys) = reverse xs ++ map (const '9') ys
    g (xs,[]) = (xs,[])
    g ([],y:ys) = ([pred y],ys)
    g (x:xs,y:ys)
      | x == y = g (xs,x:y:ys)
      | otherwise = (pred y:x:xs,ys)
    f (xs,[]) = (xs,[])
    f (x:xs,y:ys) 
      | x > y = (xs,x:y:ys)
      | otherwise = f (y:x:xs,ys)

showCase (i,xs) = concat $ ["Case #",show i,": ",xs]

byLines f = interact $ unlines . f . lines

main = byLines $ map showCase . zip [1..] . map tidy . g
  where g (l:ls) = take t ls where t = read l
