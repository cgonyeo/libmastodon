{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Mastodon.Lib where

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Types.Status as HTTPTypes
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Control.Concurrent.MVar

import GHC.Generics

import Data.Aeson

import Web.Mastodon.Lib.Types
import Web.Mastodon.Lib.Internal

data UserData = UserData
    { server_uri :: T.Text
    }



--logInAsUser :: AppInfo -> T.Text -> T.Text -> IO (Either UserData String)
--logInAsUser server username password = do
--    let oauth = newOAuth 
--                    { oauthServerName = server
--                    , oauthRequestUri = server
--                    , oauthAccessTokenUri = server
--                    , oauuthAuthorizeUri = server
--                    , oauthConsumerKey = username
--                    , oauthConsumerSecret = password
--                    , 


--getCurrentUser :: UserData -> IO (Either Account String)
--getCurrentUser u = uninplemented
