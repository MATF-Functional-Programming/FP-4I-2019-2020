import qualified Data.Char as Ch

-- Napisati funkciju safeHead koja bezbedno vraca glavu liste

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


-- Definisati funkciju safeHead tako da vraca nisku sa opisom 
-- greske u slucaju prazne liste a u opstem slucaju glavu

safeHead' :: [a] -> Either String a
safeHead' []    = Left "Prazna lista"
safeHead' (x:_) = Right x


-- Definisati funkciju koja ucitava i ispisuje liniju sa
-- standardnog ulaza

echo :: IO ()
echo = getLine >>= putStrLn


-- Napisati program koji ucitava putanju do fajla i vraca listu
-- reci u fajlu 

rf :: String -> IO [String]
rf path = readFile path >>= return . words
