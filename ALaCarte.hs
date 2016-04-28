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

module ALaCarte where

infixr 5 :+:

-- |Coproduct of functors
data (f :+: g) a = Inl (f a) | Inr (g a) deriving (Show, Eq)


instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap = undefined

class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a
  prj :: g a -> Maybe (f a)


-- Injections
instance {-# OVERLAPS #-} (Functor f) => f :<: f where
  inj = undefined
  prj = undefined

instance {-# OVERLAPS #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = undefined
  prj = undefined

instance {-# OVERLAPS #-} (Functor f, Functor g, Functor h, f :<: h) => f :<: (g :+: h) where
  inj = undefined
  prj = undefined

inject :: (f :<: g) => f (Free g a) -> Free g a
inject = undefined

-- |Free monad
data Free f a = Pure a | Free (f (Free f a)) deriving Functor

deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)
deriving instance (Eq a, Eq (f (Free f a))) => Eq (Free f a)

instance Functor f => Applicative (Free f) where
  pure  = undefined
  (<*>) = undefined

instance Functor f => Monad (Free f) where
  (>>=) = undefined

foldFree :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
foldFree = undefined

-- Interpreting, using a type class to compose interpretations

class (Functor f, Monad m) => Interpret f m where
  intp :: f a -> m a

instance (Interpret f m, Interpret g m) => Interpret (f :+: g) m where
  intp = undefined

interpret :: (Interpret f m) => Free f a -> m a
interpret = undefined

-- Interpreting, but composing interpretations manually

type f ~> g = forall a. f a -> g a

interpret' :: (Functor f, Monad m) => (f ~> m) -> Free f a -> m a
interpret' = undefined

