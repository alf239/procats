{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

module Main where

f :: Int -> Int
f x = x * x

g :: Int -> Int
g = (+ 1)

semi :: (a -> b) -> (b -> c) -> a -> c
semi p q = q . p

h :: Int -> Int
h = f . g

i :: Int -> Int
i = f `semi` g

class Category obj mor | mor -> obj where
    dom :: mor -> obj
    cod :: mor -> obj
    idy :: obj -> mor
    cmp :: mor -> mor -> Maybe mor

data ToyObj = One | Two
data ToyMor = Id1 | Id2 | F

toydom :: ToyMor -> ToyObj
toydom Id1 = One
toydom Id2 = Two
toydom F   = One

toycod :: ToyMor -> ToyObj
toycod Id1 = One
toycod Id2 = Two
toycod F   = Two

toyid :: ToyObj -> ToyMor
toyid One = Id1
toyid Two = Id2

toycmp :: ToyMor -> ToyMor -> Maybe ToyMor
toycmp Id1 Id1 = Just Id1
toycmp Id1 F   = Just F
toycmp F   Id2 = Just F
toycmp Id2 Id2 = Just Id2
toycmp _   _   = Nothing

instance Category ToyObj ToyMor where
    dom = toydom
    cod = toycod
    idy = toyid
    cmp = toycmp

main :: IO ()
main = do putStrLn "PS1 Q1"
          print $ h 2
          print $ i 2

