{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppMonad
    ( AppMonad
    , runAppMonad
    , AppError (..)
    , AppState (..)
    , EnvConfig (..)
    , liftIO
    ) where

import NanoLeafApi.Types (AuthToken)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Text.Parsec (ParseError)
import Network.HTTP.Client (Manager, HttpException)

data AppState = AppState
    { authToken :: Maybe AuthToken }
    deriving (Eq, Show)

data EnvConfig = EnvConfig
    { configAuthToken :: Maybe AuthToken 
    , connManager :: Manager }

instance Show EnvConfig where
    show (EnvConfig authTok _) = "EnvConfig " ++ show authTok

data AppError = 
    NanoLeafsNotFound | 
    AvahiBrowseCommandException String |
    AvahiBrowseParseError ParseError |
    RequestWithoutAuthToken |
    HttpError HttpException--TODO: make sure try catches for this one exist
    deriving (Show)

--Stolen from https://mmhaskell.com/blog/2022/3/24/making-your-own-monad
newtype AppMonad a = AppMonad (StateT AppState (ReaderT EnvConfig (ExceptT AppError IO)) a)
  deriving (Functor, Applicative, Monad, MonadError AppError, MonadReader EnvConfig)

runAppMonad :: AppMonad a -> EnvConfig -> AppState -> IO (Either AppError a)
runAppMonad (AppMonad stateAction) envConfig appState = 
    runExceptT (runReaderT (evalStateT stateAction appState) envConfig)

instance MonadIO AppMonad where
  liftIO = AppMonad . lift . lift . lift

--instance MonadState AppState AppMonad where
--  get = AppMonad get
--  put = AppMonad . put
