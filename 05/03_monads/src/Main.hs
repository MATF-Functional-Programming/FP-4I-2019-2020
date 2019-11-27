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


data Maybe a = Nothing
             | Just a
             deriving (Show, Eq)

instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    pure = Just

    (<*>) Nothing _ = Nothing
    (<*>) _ Nothing = Nothing
    (<*>) (Just f) (Just x) = Just (f x)

stringToUpper :: String -> Maybe String
stringToUpper s = Just $ fmap Ch.toUpper s

stringToLower :: String -> Maybe String
stringToLower s = Just $ Ch.toLower <$> s

makeBold :: String -> Maybe String
makeBold s = Just $ "<b>" ++ s ++ "</b>"

-- Kako da napravimo kompoziciju ovih parcijalnih funkcija?
-- fmap nije dovoljan

instance Monad Maybe where
    -- return :: a -> Maybe a
    return x = Just x

    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _      = Nothing
    Just x  >>= f      = f x

    -- (>>) :: Maybe a -> Maybe b   -> Maybe b

-- Monad laws
-- Left id:   return a >>= k                 == k a
-- Right id:  m        >>= return            == m
-- Assoc:     m        >>= (\x -> k x >>= h) == (m >>= k) >>= h


-- Pierre primer :)

type Birds = Int  
type Pole = (Birds,Birds)  

-- inicijalna implementacija
landLeft' :: Birds -> Pole -> Pole  
landLeft' n (left,right) = (left + n,right)  
  
landRight' :: Birds -> Pole -> Pole  
landRight' n (left,right) = (left,right + n)  

-- ghci> landLeft' 2 (0,0)  
-- (2,0)  
-- ghci> landRight' 1 (1,2)  
-- (1,3)  
-- ghci> landRight' (-1) (1,2)  
-- (1,1)  

-- ghci> landLeft' 2 (landRight' 1 (landLeft' 1 (0,0)))  
-- (3,1)  

x -: f = f x 

-- ghci> 100 -: (*3)  
-- 300  
-- ghci> True -: not  
-- False  
-- ghci> (0,0) -: landLeft' 2  
-- (2,0)  

-- ghci> (0,0) -: landLeft' 1 -: landRight' 1 -: landLeft' 2  
-- (3,1)  


-- sta ako zelimo da proverimo balans?

-- ghci> landLeft' 10 (0,3)  
-- (10,3)  
-- ghci> (0,0) -: landLeft' 1 -: landRight' 4 -: landLeft' (-1) -: landRight' (-2)  
-- (0,2) 

-- ovo nije dobro! greska se desila negde i izgubila se


-- implementacija land* funkcija tako da kontrolisu balans

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  

-- ghci> landLeft 2 (0,0)  
-- Just (2,0)  
-- ghci> landLeft 10 (0,3)  
-- Nothing  
-- ghci> landRight 1 (0,0) >>= landLeft 2  
-- Just (2,1)
-- ghci> Nothing >>= landLeft 2  
-- Nothing    
-- ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
-- Just (2,4)  

-- ako uzmemo onaj problematicni primer opet:
-- ghci> (0,0) -: landLeft' 1 -: landRight' 4 -: landLeft' (-1) -: landRight' (-2)  
-- (0,2)  
-- ghci> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)  
-- Nothing

banana :: Pole -> Maybe Pole  
banana _ = Nothing  

-- ghci> return (0,0) >>= landLeft 1 >>= banana >>= landRight 1  
-- Nothing  
-- ghci> return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1  
-- Nothing  