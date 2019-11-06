module Main where

main :: IO ()
main = do
  putStrLn "hello world"


-- Gardovi su lepsi zapis za lanac if else if else if naredbi
accumulate :: (Eq a, Num a) => (a -> a -> a) -> [a] -> a
accumulate f xs
        | xs == []   = 0
        | otherwise  = f (head xs) (accumulate f $ tail xs)

-- Mogu da sadrze i pattern matching izraze
-- (linije koje sadrze <- xs)
accumulate' :: (Eq a, Num a) => (a -> a -> a) -> [a] -> a
accumulate' f xs
        | xs == []       = 0
        | (x:[])  <- xs  = x
        | (x:xs)  <- xs  = f x (accumulate' f xs)

