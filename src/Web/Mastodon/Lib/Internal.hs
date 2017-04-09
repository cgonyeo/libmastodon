{-# LANGUAGE OverloadedStrings #-}

module Web.Mastodon.Lib.Internal where

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Types.Status as HTTPTypes
import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Data.Aeson

import Control.Concurrent.MVar

import Web.Mastodon.Lib.Types
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)

toText :: LBS.ByteString -> T.Text
toText = E.decodeUtf8 . LBS.toStrict

registerReqBody :: LBS.ByteString -> HTTP.RequestBody
registerReqBody app_name = HTTP.RequestBodyLBS $ 
    "client_name=" `LBS.append` app_name
    `LBS.append` "&redirect_uris=urn:ietf:wg:oauth:2.0:oob"
    `LBS.append` "&scopes=read write follow"

responseChecker :: MVar HTTPTypes.Status
                -> HTTP.Request
                -> HTTP.Response HTTPClient.BodyReader
                -> IO ()
responseChecker mvres _ resp = putMVar mvres (HTTPClient.responseStatus resp)

registerApp :: LBS.ByteString -> LBS.ByteString -> ExceptT MError IO AppInfo
registerApp server app_name = do
    mvres <- liftIO newEmptyMVar
    request' <- liftIO $ (HTTP.parseRequest ("https://" ++ LBS.unpack server ++ "/api/v1/apps"))
    let request = request'
                   { HTTP.method        = "POST"
                   , HTTP.secure        = True
                   , HTTP.requestBody   = registerReqBody app_name
                   , HTTP.checkResponse = responseChecker mvres
                   }
    manager <- liftIO $ HTTP.newManager HTTP.tlsManagerSettings
    res <- liftIO $ HTTP.httpLbs request manager  
    status <- liftIO $ takeMVar mvres
    case (status == HTTPTypes.status200, parseAppInfo server res) of
        (True,Just ai) -> return ai
        (True,Nothing) -> throwE $ MError "error decoding registration info"
        _ -> throwE $ MError ("something has gone horribly wong: " ++ show status)

parseAppInfo :: LBS.ByteString -> HTTP.Response LBS.ByteString -> Maybe AppInfo
parseAppInfo server res = 
    case decode (HTTP.responseBody res) of
        Just ai -> Just (appRegToAppInfo (toText server) ai)
        Nothing -> Nothing

startAuthWithLmrs :: AppInfo -> ExceptT MError IO AuthCode
