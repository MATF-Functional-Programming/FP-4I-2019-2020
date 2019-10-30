-- Ukloniti svaki n-ti element iz liste
-- remn [1,2,3,4,5] 2 = [1,3,5]

remn :: [a] -> Int -> [a]
remn list count = helper list count count
  where helper [] _ _ = []
        helper (x:xs) count 1 = helper xs count count
        helper (x:xs) count n = x : (helper xs count (n - 1))

remn' :: [a] -> Int -> [a]
remn' xs n = helper xs n
    where helper [] _ = []
          helper (x:xs) 1 = helper xs n
          helper (x:xs) k = x : helper xs (k-1)

remn'' :: [a] -> Int -> [a]
remn'' xs n = map fst $ filter (\(x,i) -> i `mod` n /= 0) $ zip xs [1..]