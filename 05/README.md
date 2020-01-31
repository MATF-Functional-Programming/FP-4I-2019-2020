# Functors, Applicative functors, Monads

## Functors
- `Functor` - sve sto implementira `fmap` ili `<$>` (infiksni `fmap`)
- `kind`, za `Functor`, je `* -> *` (videti: `:info Functor`) 
- Stoga `instance Functor Either` ne moze, vec `instance Functor (Either a)` - mora biti tacno **1** tipski parametar
- Neki funktori: `Maybe a`, `Either a b`, `[]`, `IO` (svi su oni vise od funktora ali o tome kasnije)
- `(->) r` je takodje funktor. Ali sta je to?
    `r -> a` se moze pisati prefiksno kao `(->) r a` - funkcija 2 argumenta, stoga se mora primeniti delimicno zbog 
    kind-a Functor klase.
    U `Control.Monad.Instances` se nalazi implementacija

    ```hs
    instance Functor ((->) r) where
        fmap f g = (\x -> f (g x))
    ```
        
    - Tumacenje:
        - Krecemo od potpisa za fmap: `fmap :: (a -> b) -> f a -> f b`
        - Zameniti `f` sa `((->) r)`: `fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`
        - Napisemo infiksno: `fmap :: (a -> b) -> (r -> a) -> (r -> b)`
        - Podseca na nesto? Kompozicija funkcija!

    ```hs
    instance Functor ((->) r) where
        fmap f g = (.)
    ```   
    
- Primeri: 
    ```hs
    ghci> :t fmap (*3) (+100)  
    fmap (*3) (+100) :: (Num a) => a -> a  
    
    ghci> fmap (*3) (+100) 1  
    303  
    
    ghci> (*3) `fmap` (+100) $ 1  
    303  
    
    ghci> (*3) . (+100) $ 1  
    303  
    
    ghci> fmap (show . (*3)) (*100) 1  
    "300"  
    ```
- `fmap` se u karijevskom stilu vidi kao: `fmap :: (a -> b) -> (f a -> f b)` 
- Ovo se naziva _lifting_ - podizemo funkciju jednog argumenta da radi nad funktorima
    ```hs
    ghci> :t fmap (*2)  
    fmap (*2) :: (Num a, Functor f) => f a -> f a  
    
    ghci> :t fmap (replicate 3)  
    fmap (replicate 3) :: (Functor f) => f a -> f [a]  
    
    ghci> fmap (replicate 3) [1,2,3,4]  a
    [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]  
    
    ghci> fmap (replicate 3) (Just 4)  
    Just [4,4,4]  
    
    ghci> fmap (replicate 3) (Right "blah")  
    Right ["blah","blah","blah"]  
    
    ghci> fmap (replicate 3) Nothing  
    Nothing  
    
    ghci> fmap (replicate 3) (Left "foo")  
    Left "foo"  
    ```
- Zakoni funktora:
    - `fmap id = id`
    - `fmap (f . g) = fmap f . fmap g`

### Applicative Functors
- Sta ako zelimo da mapiramo nad funkcijama 2 argumenta, npr `(*)` ili `(++)`?
    ```hs
    ghci> :t fmap (++) (Just "hey")  
    fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])  
    ```
- Sta ako imamo `Just (3*)` i `Just 5` i zelimo da uzmemo `(3*)` i mapiramo nad `Just 5`
- Nemoguce po logici funktora, zato koristimo aplikative (`Control.Applicative`)
    ```hs
    class (Functor f) => Applicative f where  
        pure :: a -> f a  
        (<*>) :: f (a -> b) -> f a -> f b  
    ```
    - Objasnjenje:
        - pure  - ubacuje u kutiju
        - `<*>` - videti `:t (<*>)`, pojacana verzija `fmap`, prvo izvuce funkciju iz funktora i primeni je

- Primeri:
    ```hs    
    ghci> Just (+3) <*> Just 9  
    Just 12  
    
    ghci> pure (+3) <*> Just 10  
    Just 13  
    
    ghci> pure (+3) <*> Just 9  
    Just 12  
    
    ghci> Just (++"hahah") <*> Nothing  
    Nothing  
    
    ghci> Nothing <*> Just "woot"  
    Nothing  
    ```
- Mozemo koristeci aplikative da mapiramo funkciju vise argumenata preko funktora
    ```hs  
    ghci> pure (+) <*> Just 3 <*> Just 5  
    Just 8  
    
    ghci> pure (+) <*> Just 3 <*> Nothing  
    Nothing  

    ghci> pure (+) <*> Nothing <*> Just 5  
    Nothing  
    ```

- `pure f <*> x` je isto sto i `fmap f x`, stoga:
    ```hs
    ghci> (++) <$> Just "johntra" <*> Just "volta"  
    Just "johntravolta"  
    ```

- `[]` je `Applicative`
    ```hs
    instance Applicative [] where  
        pure x = [x]  
        fs <*> xs = [f x | f <- fs, x <- xs]  
    ```

    Primeri:
    ```hs
    ghci> [(*0),(+100),(^2)] <*> [1,2,3]  
    [0,0,0,101,102,103,1,4,9]  
    
    ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
    [4,5,5,6,3,4,6,8] 
    ```
    
    Ne moze tip da ima 2 implementacije `pure` i `<*>` pa stoga postoji i `ZipList` koja radi 
    aplikaciju prve funkcije prve liste sa prvom vrednoscu druge liste itd.
    
- `liftA2` iz `Control.Applicative` radi lift binarne funkcije
    `liftA2 f a b = f <$> a <*> b`

    ```hs
    ghci> liftA2 (:) (Just 3) (Just [4]) 
    Just [3,4] 

    ghci> (:) <$> Just 3 <*> Just [4]  
    Just [3,4]  
    ```

### Monads
- Ako imamo vrednost sa kontekstom `m a`, kako da ga damo funkciji `a -> m b`?
- Ukoliko resimo ovaj problem, mozemo praviti i kompozicije ovakvih funkcija!
- Zelimo funkciju koja radi: `(Monad m) => m a -> (a -> m b) -> m b` 
- _Bind_ funkcija `:t (>>=)`
- **Monade** su aplikativni funktori koji definisu i bind (`>>=`)
- `:t (>>=)`
- `[]`, `Maybe`, `Either`, `[]`, `(->) r`, `IO` su monade
- Kako bismo definisali `>>=` za `Maybe`?
    ```hs
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b  
    Nothing  >>= f = Nothing  
    (Just x) >>= f = f x  
    ```
- Primeri
    ```hs 
    gchi> let f = \x -> Just (x+1)
    ghci> f 1  
    Just 2
    ghci> Just 3 >>= f 
    Just 4  
    ghci> Nothing >>= f 
    Nothing  
    ghci> Just "smile" >>= \x -> Just (x ++ " :)")  
    Just "smile :)"  
    ghci> Nothing >>= \x -> Just (x ++ " :)")  
    Nothing  
    ghci> Just 3 >>= \x -> if x > 2 then Just x else Nothing  
    Just 3  
    ghci> Just 1 >>= \x -> if x > 2 then Just x else Nothing  
    Nothing  
    ```
- `:info Monad`
    - razlika `>>` i `>>=`, kada koristiti koji?
    - `>>` dolazi sa podrazumevanom implementacijom
        ```hs
        ghci> Nothing >> Just 3  
        Nothing  
        ghci> Just 3 >> Just 4  
        Just 4  
        ghci> Just 3 >> Nothing  
        Nothing  
        ```
    - `return` isto sto i `pure`
    - `fail` se ne koristi u nasim kodovima, vec se koristi unutar jezika
- Kako bismo "dokazali" da je `Maybe` instanca `Monad` klase?
    ```hs
    instance Monad Maybe where  
        return x = Just x  
        Nothing >>= f = Nothing  
        Just x >>= f  = f x  
        fail _ = Nothing  
    ```
- Kako bismo "dokazali" da je `[]` instanca `Monad` klase?
    ```hs
    instance Monad [] where  
        return x = [x]  
        xs >>= f = concat (map f xs)  
        fail _ = []  

    ghci> [3,4,5] >>= \x -> [x,-x]  
    [3,-3,4,-4,5,-5]  
    ```
- Zakoni:
    - _levi identitet_ : `return x >>= f`  je isto sto i `f x` (_levi identitet_)
    - _desni identitet_: `m >> return`     je isto sto i `m`   (_desni identitet_)
    - _asocijativnost_ : `(m >>= f) >>= g` je isto sto i `m >>= (\x -> f x >>= g)` 
- Videti primer sa vezbi
- Postoji specijalna notacija u sintaksi Haskell-a za monade zvana `do` notacija
    ```hs
    foo :: Maybe String  
    foo = Just 3   >>= (\x -> 
          Just "!" >>= (\y -> 
          Just (show x ++ y)))  
    
    foo :: Maybe String  
    foo = do  
        x <- Just 3  
        y <- Just "!"  
        Just (show x ++ y) 
    ```
