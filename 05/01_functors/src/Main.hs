module Main where

import Prelude hiding ( Maybe, Nothing, Just
                      , Either, Left, Right
                      )
import qualified Data.Char as Ch
import Debug.Trace (trace)
import Data.Bifunctor

main :: IO ()
main = do
  putStrLn "hello world"


-- Funktor -- bilo sta sto ima fmap
-- Alternativno ime za fmap je <$>

stringToUpper :: String -> String
stringToUpper s = fmap Ch.toUpper s

stringToLower :: String -> String
stringToLower s = Ch.toLower <$> s

-- Pravila

-- fmap id = id
-- fmap (f . g) == fmap f . fmap g


-- Neki korisni funktori ----------------



-- Tip za baratanje opcionim vrednostima

data Maybe a = Nothing
             | Just a
             deriving (Show, Eq)


-- Parcijalna funkcija
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x


-- Funkcija sa podrazumevanom/opcionom vrednoscu argumenta
debug :: Show a => a -> Maybe String -> a
debug value message = trace (fromMaybe "" message ++ show value) value


-- U nekim bibliotekama postoji ?: koji je flip fromMaybe
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just x) = x


-- Instanca funktora nad Maybe

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)


makeBold :: String -> String
makeBold s = "<b>" ++ s ++ "</b>"


currentUserName :: Maybe String
currentUserName = Just "John Doe"


formattedUserName :: Maybe String -> Maybe String
formattedUserName username =
                    fmap makeBold $ fmap stringToUpper username


formattedUserName' :: Maybe String -> Maybe String
formattedUserName' = (fmap makeBold) . (fmap stringToUpper)



-- Tip ili-ili
-- Cesto se koristi za baratanje greskama
-- gde je Left greska, a Right ispravna vrednost

data Either a b = Left a
                | Right b
                deriving (Show, Eq)

type StrErr a = Either String a

safeHeadVerbose :: [a] -> StrErr a
safeHeadVerbose []     = Left "The list is empty"
safeHeadVerbose (x:xs) = Right x


maybeToErr :: Maybe a -> StrErr a
maybeToErr Nothing  = Left "Unknown error"
maybeToErr (Just x) = Right x


errToMaybe :: StrErr a -> Maybe a
errToMaybe (Left _)  = Nothing
errToMaybe (Right x) = Just x


instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left e)  = Left e


currentUserNameVerbose :: StrErr String
currentUserNameVerbose = Right "Jane Doe"


formattedUserNameVerbose :: StrErr String -> StrErr String
formattedUserNameVerbose username =
                    fmap makeBold $ fmap stringToUpper username







formattedUserNameUniversal :: Functor f => f String -> f String
formattedUserNameUniversal username =
                    fmap makeBold $ fmap stringToUpper username



------ formattedUserNameUniversal Nothing
------ formattedUserNameUniversal (Just "John")
------ formattedUserNameUniversal (Left "Err")
------ formattedUserNameUniversal (Right "Jane")
------ formattedUserNameUniversal []
------ formattedUserNameUniversal ["John", "Jane"]
------ formattedUserNameUniversal getLine
------ formattedUserNameUniversal (4, "Tom Baker")



-- Ako zelimo da imamo mapiranje nad oba tipa za Either,
-- mozemo da koristimo bifunktore

instance Bifunctor Either where
    bimap f g (Left x)  = Left (f x)
    bimap f g (Right y) = Right (g y)

    first f (Left x)  = Left (f x)
    first f (Right y) = Right y

    second g (Left x)  = Left x
    second g (Right y) = Right (g y)


-- Coordinate example




