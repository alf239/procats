{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

module Main where

import Data.Either

class Category obj mor | mor -> obj where
    dom :: mor -> obj
    cod :: mor -> obj
    idy :: obj -> mor
    cmp :: mor -> mor -> Maybe mor

-- instance Monoid mor => (Category () mor) where
--     dom _ = ()
--     cod _ = ()
--     idy _ = mempty
--     cmp m n = Just (m <> n)

data BIMorph = BB (Bool -> Bool) | BI (Bool -> Int) | II (Int -> Int) | IB (Int -> Bool) 

instance Category (Either () ()) BIMorph where
    dom (BB _) = Left ()
    dom (BI _) = Left ()
    dom (IB _) = Right ()
    dom (II _) = Right ()
    cod (BB _) = Left ()
    cod (BI _) = Right ()
    cod (IB _) = Left ()
    cod (II _) = Right ()
    idy (Left ()) = BB id
    idy (Right ()) = II id
    cmp (BB f) (BB g) = Just $ BB (f . g)
    cmp (BI f) (BB g) = Just $ BI (f . g)
    cmp (II f) (BI g) = Just $ BI (f . g)
    cmp (IB f) (BI g) = Just $ BB (f . g)
    cmp (II f) (II g) = Just $ II (f . g)
    cmp (IB f) (II g) = Just $ IB (f . g)
    cmp (BB f) (IB g) = Just $ IB (f . g)
    cmp (BI f) (IB g) = Just $ II (f . g)
    cmp _ _ = Nothing


main = do
    let a = 5
    let b = 2
    print $ a `div` b


