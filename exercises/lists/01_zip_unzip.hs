-- Rucno implementirati funkcije zip, zipWith i unzip

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip'   _      _    = []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys
zipWith' _   _      _    = []

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' xs = (map fst xs, map snd xs)