module NanoLeafApi.Types
    ( AuthToken 
    , mkAuthToken
    , getAuthToken )
    where

import Data.Text (Text)

newtype AuthToken = AuthToken
    { getAuthToken :: Text }
    deriving (Show, Eq)

mkAuthToken :: Text -> AuthToken
mkAuthToken = AuthToken
