{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

module Lecture2 where

class Category obj mor where
    dom :: mor -> obj
    cod :: mor -> obj
    idy :: obj -> mor
    cmp :: mor -> mor -> Maybe mor

instance Monoid mor => (Category () mor) where
    dom _ = ()
    cod _ = ()
    idy _ = mempty
    cmp m n = Just (m <> n)

main = do
    let a = 5
    let b = 2
    print $ a `div` b


