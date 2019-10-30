-- Razdvojiti listu na dva dela, gde je n duzina prvog
-- split [1..12] 5 = ([1,2,3,4,5],[6,7,8,9,10,11,12])

split :: [a] -> Int -> ([a], [a])
split = flip splitAt

split' :: [a] -> Int -> ([a], [a])
split' xs n = (take n xs, drop n xs)

split'' :: [a] -> Int -> ([a], [a])
split'' xs n = if n < 0 then ([], xs) else splitR n xs []
    where 
        splitR 0 xs accum = (reverse accum, xs)
        splitR _ [] accum = (reverse accum, [])
        splitR n (x:xs) accum = splitR (n-1) xs (x : accum)