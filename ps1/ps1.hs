module Main where

f :: Int -> Int
f x = x * x

g :: Int -> Int
g = (+ 1)

semi :: (a -> b) -> (b -> c) -> a -> c
semi f g = g . f

main :: IO ()
main = let h = f . g
           i = f `semi` g
       in do putStrLn "PS1 Q1"
             print $ h 2
             print $ i 2
