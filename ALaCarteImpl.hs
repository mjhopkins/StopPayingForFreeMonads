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

module ALaCarteImpl where

import ALaCarte
import ALaCarteDSL

import Control.Monad.State
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Map


-- calculator language impl

instance MonadState Int m => Interpret Clear m where
 intp = undefined

instance MonadState Int m => Interpret Recall m where
 intp = undefined

instance MonadState Int m => Interpret Incr m where
 intp = undefined


 -- console language impl

instance MonadIO m => Interpret Ask m where
 intp = undefined

instance MonadIO m => Interpret Tell m where
 intp = undefined


-- key-value

instance (Eq k, MonadReader [(k, v)] m) => Interpret (Lookup k v) m where
  intp = undefined


-- run test programs at the REPL

sayHello :: IO ()
sayHello = interpret (sayHelloProg :: Free (Ask :+: Tell) ())

runTick :: State Int Int
runTick = interpret (tick :: Free (Recall :+: Incr) Int)

calcEx1 :: StateT Int IO Int
calcEx1 = interpret prog
  where
    prog :: Free (Clear :+: Recall :+: Incr) Int
    prog = do
         incr 12
         t <- recall
         clear
         return t

calcEx2 :: StateT Int IO Int
calcEx2 = interpret prog
  where
  prog :: Free (Clear :+: Recall :+: Incr :+: Tell :+: Ask) Int
  prog = do
    -- replace with something interesting.
    -- e.g. ask the user for an action
    -- do it and print the result
    return 0

runLookupProg :: ReaderT [(String, Int)] IO ()
runLookupProg = interpret lookupProg

-- add a test interpreter for the console language that takes an
-- input map of canned responses and returns the list of outputs
-- that the program produces

testSayHello :: Map String String -> [String]
testSayHello = undefined

-- run a test program using the manual style of composing interpretations

interpret'_ex :: State Int Int
interpret'_ex = interpret' n prog
  where
    -- or add your own more interesting program
    prog :: Free (Clear :+: Recall :+: Incr) Int
    prog = do
      incr 12
      t <- recall
      clear
      return t

    n :: (Clear :+: Recall :+: Incr) ~> State Int
    n = undefined
--     n = cn `sumF` (rn `sumF` ni)

    cn :: Clear ~> State Int
    cn = undefined
    rn :: Recall ~> State Int
    rn = undefined
    ni :: Incr ~> State Int
    ni = undefined



