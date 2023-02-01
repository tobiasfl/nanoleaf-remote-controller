{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppMonad
    ( AppMonad
    , runAppMonad
    , AppError (..)
    , AppState (..)
    , EnvConfig (..)
    , liftIO
    ) where

import Types (AuthToken)
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
    , connectionManager :: Manager }

instance Show EnvConfig where
    show (EnvConfig authTok _) = "EnvConfig " ++ show authTok

data AppError = 
    NanoLeafsNotFound | 
    AvahiBrowseCommandException String |
    AvahiBrowseParseError ParseError |
    RequestWithoutAuthToken |
    HttpRequestError HttpException | --TODO: make sure try catches for this one exist
    NanoLeafHttpErrorResponse --TODO: make sure this one is thrown when response status is not 200 
    deriving (Show)

newtype AppMonad a = AppMonad (StateT AppState (ReaderT EnvConfig (ExceptT AppError IO)) a)
  deriving (Functor, Applicative, Monad, MonadError AppError, MonadReader EnvConfig)

runAppMonad :: AppMonad a -> EnvConfig -> AppState -> IO (Either AppError a)
runAppMonad (AppMonad stateAction) envConfig appState = 
    runExceptT (runReaderT (evalStateT stateAction appState) envConfig)

instance MonadIO AppMonad where
  liftIO = AppMonad . lift . lift . lift
