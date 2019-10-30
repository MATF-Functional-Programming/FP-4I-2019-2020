-- Izvuci podlistu od indeksa i do indeksa j
-- slice [1..10] 2 4 = [2,3]

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j-i+1) $ drop (i-1) xs

slice' :: [a] -> Int -> Int -> [a]
slice' xs i j = snd $ splitAt (i-1) $ fst $ splitAt j xs

slice'' :: [a] -> Int -> Int -> [a]
slice'' xs i j = map snd
               $ filter (\(x,_) -> x >= i && x <= j)
               $ zip [1..] xs

slice''' :: [a] -> Int -> Int -> [a]
slice''' xs i j = [x | (x,index) <- zip xs [1..j], i <= index]

slice'''' :: [a] -> Int -> Int -> [a]
slice'''' xs i j
  | i > j = []
  | otherwise = (take (j-i+1) (drop (i-1) xs))