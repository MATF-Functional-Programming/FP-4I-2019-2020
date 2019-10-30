-- Duplirati elemente liste
-- dup [1,2,3] = [1,1,2,2,3,3]

dup :: [a] -> [a]
dup []     = []
dup (x:xs) = x : x : dup xs

dup' :: [a] -> [a]
dup'  xs = concat [[x,x] | x <- xs]

dup'' :: [a] -> [a]
dup''    = concatMap (\x -> [x,x])

dup''' :: [a] -> [a]
dup'''   = concatMap (replicate 2)

dupfl :: [a] -> [a]
dupfl    = foldl (\acc x -> acc ++ [x,x]) []

dupfr :: [a] -> [a]
dupfr    = foldr (\x acc -> x : x : acc)  []


-- Ponoviti svaki element liste n puta
-- rep [1,2,3] 3 = [1,1,1,2,2,2,3,3,3]

rep :: [a] -> Int -> [a]
rep xs n = concatMap (replicate n) xs

rep' :: [a] -> Int -> [a]
rep' = flip $ concatMap . replicate

rep'' :: [a] -> Int -> [a]
rep'' xs n = concatMap (take n . repeat) xs

rep''' :: [a] -> Int -> [a]
rep''' []     _ = []
rep''' (x:xs) n = foldr (const (x:)) (rep''' xs n) [1..n]

repfl :: [a] -> Int -> [a]
repfl xs n = foldl (\acc x -> acc ++ repHelp x n) [] xs
    where
      repHelp _ 0 = []
      repHelp x n = x : repHelp x (n-1)
    