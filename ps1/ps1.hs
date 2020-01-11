{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

module Main where

f :: Int -> Int
f x = x * x

g :: Int -> Int
g = (+ 1)

semi :: (a -> b) -> (b -> c) -> a -> c
semi f g = g . f


class Category obj mor | mor -> obj where
    dom :: mor -> obj
    cod :: mor -> obj
    idy :: obj -> mor
    cmp :: mor -> mor -> Maybe mor

data Obj2 = One | Two
data Mor2 = Id1 | Id2 | F

instance Category Obj2 Mor2 where
    dom Id1 = One
    dom Id2 = Two
    dom F = One
    cod Id1 = One
    cod Id2 = Two
    cod F = Two
    idy One = Id1
    idy Two = Id2
    cmp Id1 F = Just F
    cmp F Id2 = Just F
    cmp _ _ = Nothing

main :: IO ()
main = let h = f . g
           i = f `semi` g
       in do putStrLn "PS1 Q1"
             print $ h 2
             print $ i 2

