-- Ukloniti element iz liste na poziciji n
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n xs 
    | n > 0 && n <= length xs = (Just (xs !! index), take index xs ++ drop n xs)
    | otherwise = (Nothing, xs)
    where index = n - 1

-- Ubaciti element na poziciju u listi
insertAt :: a -> [a] -> Int -> [a]
insertAt e lst pos = foldr concat' [] $ zip [1..] lst
    where
        concat' (i, x) xs
            | i == pos  = e:x:xs
            | otherwise = x:xs
