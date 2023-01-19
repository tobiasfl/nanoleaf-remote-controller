{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppMonad
    ( --AppMonad 
    ) where

import NanoLeafApi.Types (AuthToken)
import Types (NanoLeaf)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

data AppState = AppState
    { authToken :: AuthToken
    , nanoLeaf :: NanoLeaf}
    deriving (Eq, Show)

data EnvConfig = EnvConfig
    {  }
    deriving (Eq, Show)

data AppError = NanoLeafsNotFound
    deriving (Eq, Show)


--Stolen from https://mmhaskell.com/blog/2022/3/24/making-your-own-monad
--newtype AppMonad a = AppMonad (StateT AppState (ReaderT EnvConfig (ExceptT AppError IO))) a
--  deriving (Functor, Applicative, Monad)
--
--runAppMonad :: AppMonad a -> EnvConfig -> AppState -> IO (Either AppError a)
--runAppMonad (AppMonad stateAction) envConfig appState = ioAction
--  where
--    readerAction :: ReaderT (ExceptT AppError IO) a
--    readerAction = evalStateT stateAction appState
--
--    exceptAction :: ExceptT AppError IO a
--    exceptAction = runReaderT readerAction envConfig
--
--    ioAction :: IO (Either AppError a)
--    ioAction = runExceptT exceptAction
--
--instance MonadIO AppMonad where
--  liftIO = AppMonad . lift . lift . lift
--
--instance MonadState AppState AppMonad where
--  get = AppMonad get
--  put = AppMonad . put
