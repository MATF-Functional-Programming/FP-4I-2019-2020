-- Rotirati listu za n mesta u levo
-- rotate [1,2,3,4,5] 2 = [3,4,5,1,2]

rotate :: [a] -> Int -> [a]
rotate xs n = drop nn xs ++ take nn xs
    where nn = n `mod` length xs

rotate' :: [a] -> Int -> [a]
rotate' xs n = take (length xs) $ drop n $ cycle xs