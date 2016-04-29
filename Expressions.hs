{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE GADTs                 #-}

module Expressions where

import ALaCarte hiding (inject)


-- extra functor coproduct stuff

{-| Utility function to case on a functor sum, without exposing the internal
  representation of sums. -}
caseF :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
{-# INLINE caseF #-}
caseF f g x = case x of
                Inl x -> f x
                Inr x -> g x

projL :: (f :+: g) a -> Maybe (f a)
projL s = case s of
  Inl fa -> Just fa
  Inr _  -> Nothing

projR :: (f :+: g) a -> Maybe (g a)
projR s = case s of
  Inl _  -> Nothing
  Inr ga -> Just ga

-- Fix

newtype Fix f = Fix { unfix :: f (Fix f) }

deriving instance (Show (f (Fix f))) => Show (Fix f)
deriving instance (Eq (f (Fix f))) => Eq (Fix f)


inject :: (f :<: g) => f (Fix g) -> Fix g
inject = undefined

project :: (f :<: g) => Fix g -> Maybe (f (Fix g))
project = undefined


------- Simple language of arithmetic expressions

data Val a = Val Int deriving (Functor, Show, Eq)
data Add a = Add a a deriving (Functor, Show, Eq)
data Mul a = Mul a a deriving (Functor, Show, Eq)


type IntExpr = Fix Val
type AddExpr = Fix Add

intExpr :: IntExpr
intExpr = Fix (Val 3)

addExpr :: AddExpr
addExpr = Fix (Add addExpr addExpr)

-- add some smart constructors


val :: (Val :<: f) => Int -> Fix f
val = undefined

(|+|) :: (Add :<: f) => Fix f -> Fix f -> Fix f
x |+| y = undefined

(|*|) :: (Mul :<: f) => Fix f -> Fix f -> Fix f
x |*| y = undefined

fold :: Functor f => (f a -> a) -> Fix f -> a
fold f (Fix t) = undefined

---------


addExpr2 :: Fix (Add :+: Val)
addExpr2 = val 33 |+| val 9


----- evaluating
-----
----- an "algebra" for a functor f
----- is just a function f a -> a
-----

class Functor f => Alg f a where
  ev :: f a -> a

instance Alg Val Int where
  ev (Val v) = v

instance Alg Add Int where
  ev (Add x y) = undefined

instance Alg Mul Int where
  ev (Mul x y) = undefined

instance (Alg f x, Alg g x) => Alg (f :+:g) x where
  ev = undefined

class Functor f => Eval f where
  evalAlg :: f Int -> Int

instance Eval Val where
  evalAlg (Val v) = undefined

instance Eval Add where
  evalAlg (Add x y) = undefined

instance Eval Mul where
  evalAlg (Mul x y) = undefined

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlg = undefined

eval :: Eval f => Fix f -> Int
eval = undefined


mulExpr :: Fix (Val :+: Mul)
mulExpr = val 42 |*| val 3

mulExpr_eval :: Int
mulExpr_eval = eval mulExpr


---- add a "pretty print" operation along the lines of eval


class Functor f => Print f where
--- implement

printExpr :: Print f => Fix f -> String
printExpr = undefined

-- optional:
-- how would you apply the algebraic transformation
-- x(a + b) = xa + xb
-- to expressions containing values, multiplications and additions?