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

module ALaCarteDSL where

import ALaCarte

-- |a  language for modelling a calculator memory
-- We can increment the memory, recall it, or clear it.
data Calculator' a where
  Incr'   :: Int -> Calculator' ()
  Recall' :: Calculator' Int
  Clear'  :: Calculator' ()

-- the same, but CPS transformed

data Calculator'' a -- implement

instance Functor Calculator'' where
  fmap = undefined

-- broken up into parts

data Incr a  -- implement
data Recall a  --implement
data Clear a -- implement

-- and put back together again

type Calculator = Incr :+: Recall :+: Clear

instance Functor Incr where
  fmap = undefined

instance Functor Recall where
  fmap = undefined

instance Functor Clear where
  fmap = undefined

-- smart constructors

incr :: (Incr :<: f) => Int -> Free f ()
incr i = undefined

recall :: (Recall :<: f) => Free f Int
recall = undefined

clear :: (Clear :<: f) => Free f ()
clear = undefined
