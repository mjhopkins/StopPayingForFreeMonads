{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ConstraintKinds            #-}

module Tagless where

import           Data.Functor.Identity
import           Data.Map.Strict as DM
import           Data.Maybe
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Catch
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.State (runStateT, StateT, gets, modify)
import           Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State as S
import           Control.Monad.Reader (runReaderT, ReaderT, MonadReader, local)
import qualified Control.Monad.Reader as R
import           Control.Monad.Writer (Writer)
import qualified Control.Monad.Writer as W
import           Prelude hiding (log)


class Console r where
  ask  :: String -> r String
  tell :: String -> r ()

instance {-# OVERLAPPABLE #-} MonadIO m => Console m where
  ask  = undefined
  tell = undefined


class KeyVal r where
  get :: String -> r (Maybe String)
  put :: String -> String -> r ()
  delete :: String -> r ()


instance (MonadState (Map String String) m) => KeyVal m where
  get = gets . DM.lookup
  put k v = modify $ DM.insert k v
  delete = modify . DM.delete


-- test program

sayHelloProg :: (Monad r, Console r) => r ()
sayHelloProg = do
  name <- ask "What is your name?"
  tell $ "Hello " ++ name


-- interpretations
sayHello :: IO ()
sayHello = sayHelloProg


testSayHello :: Map String String -> [String]
testSayHello = undefined


-- break up KeyVal into 3 separate type classes, and redefine KeyVal as a synonym


-- program with control flow

data PwdException = WrongPassword | PasswordsDidNotMatch

instance Exception PwdException
instance Show PwdException where
   show WrongPassword = "Wrong password or unknown user"
   show PasswordsDidNotMatch = "Passwords did not match"


changePwd :: (Console r, KeyVal r, MonadThrow r) => r ()
changePwd = do
  n <- ask "What's your name?"
  p <- ask "What's your password?"
  matches <- (== Just p) <$> get n
  unless matches $ throwM WrongPassword
  np <- ask "Enter new password"
  np2 <- ask "Enter new password again"
  unless (np == np2) $ throwM PasswordsDidNotMatch
  put n np
  return ()

runChangePwd :: IO ((), Map String String)
runChangePwd = undefined


-- testChangePwd :: ?
-- testChangePwd = undefined
