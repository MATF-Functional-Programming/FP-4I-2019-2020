-- 1

countGreater1st' :: Ord a => [a] -> Int
countGreater1st' [] = 0
countGreater1st' (x:xs) = countGreater1stRec x xs
    where countGreater1stRec x xs 
            | null xs = 0
            | (head xs) > x = 1 + countGreater1stRec x (tail xs)
            | otherwise = 0 + countGreater1stRec x (tail xs)

countGreater1st'' :: Ord a => [a] -> Int
countGreater1st'' [] = 0
countGreater1st'' (x:xs) = foldl (\acc e -> if e > x then acc + 1 else acc) 0 xs 

countGreater1st''' :: Ord a => [a] -> Int
countGreater1st''' [] = 0
countGreater1st''' (x:xs) = length $ filter (>x) $ xs

prop_test :: Ord a => [a] -> Bool
prop_test lst = (countGreater1st' lst) == (countGreater1st'' lst)


-- 2

hasSameChars :: String -> Bool
hasSameChars s = any (\tup -> fst tup == snd tup) $ zip s (tail s) 


-- 3

data Model = Waiting
           | Either String String


-- 4

-- whichSort :: Ord a => [a] -> String
-- whichSort lst =
--     let f = head lst   
--         l = last lst
--     in  if (f < l) then "LT"
--         else if (f > l) then "GT"
--         else "EQ"

whichSort :: Ord a => [a] -> String
whichSort lst 
    | null lst = "EQ"
    | ifAll (>=) lst = "GT" 
    | ifAll (<=) lst = "LT"
    | ifAll (==) lst  = "EQ" 
    | otherwise = "not sorted list"
    where ifAll f lst = all (\t -> f (fst t) (snd t)) $ zip lst (tail lst)

-- 5

firstLessThan :: Int -> [Int] -> Maybe Int
firstLessThan e lst = 
    if null greater then Nothing
                    else Just (snd $ head greater)
    where 
        greater = getGreaterElements e lst
        getGreaterElements e lst = filter (\t -> fst t < e) $ zip lst [0..]