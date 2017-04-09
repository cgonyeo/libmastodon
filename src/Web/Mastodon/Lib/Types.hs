{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Mastodon.Lib.Types where

import GHC.Generics
import Data.Aeson

import qualified Servant as S
import Servant.API
import Servant.Client
import Web.Internal.FormUrlEncoded

import qualified Data.Text as T

data MError = MError { err_message :: String
                     }

type AccountID = T.Text

-- The mastodon API type

type MastodonAPI =
  -- Accounts

    -- Fetching an account
       Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> Capture "id" AccountID
    :> Get '[JSON] (Either Error Account)

    -- Getting the current user
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> "verify_credentials"
    :> Get '[JSON] (Either Error Account)

    -- Getting an account's followers
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> Capture "id" AccountID
    :> "followers"
    :> Get '[JSON] (Either Error [Account])

    -- Getting who account is following
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> Capture "id" AccountID
    :> "following"
    :> Get '[JSON] (Either Error [Account])

    -- Getting an account's statuses
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> Capture "id" AccountID
    :> "statuses"
    :> QueryParam "only_media" Bool
    :> QueryParam "exclude_replies" Bool
    :> Get '[JSON] (Either Error [Status])

    -- Following/unfollowing an account
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> Capture "id" AccountID
    :> "follow"
    :> Get '[JSON] (Either Error Account)
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> Capture "id" AccountID
    :> "unfollow"
    :> Get '[JSON] (Either Error Account)

    -- Blocking/unblocking an account
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> Capture "id" AccountID
    :> "block"
    :> Get '[JSON] (Either Error Account)
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> Capture "id" AccountID
    :> "unblock"
    :> Get '[JSON] (Either Error Account)

    -- Muting/unmuting an account
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> Capture "id" AccountID
    :> "mute"
    :> Get '[JSON] (Either Error Account)
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> Capture "id" AccountID
    :> "unmute"
    :> Get '[JSON] (Either Error Account)

    -- Getting an account's relationships
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> "relationships"
    :> Get '[JSON] (Either Error [Relationship])

    -- Searching for accounts
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "accounts"
    :> "search"
    :> QueryParam "q" T.Text
    :> QueryParam "limit" Int
    :> Get '[JSON] (Either Error [Account])

  -- Apps

    -- Registering an application
  :<|> "api" :> "v1"
    :> "apps"
    :> ReqBody '[FormUrlEncoded] AppRegisterBody
    :> Post '[JSON] (Either Error AppRegistration)

  -- Blocks

    -- Fetching a user's blocks
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "blocks"
    :> Get '[JSON] (Either Error [Account])

  -- Favourites

    -- Fetching a user's favourites
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "favourites"
    :> Get '[JSON] (Either Error [Status])

  -- Follow Requests

    -- Fetching a list of follow requests
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "follow_requests"
    :> Get '[JSON] (Either Error [Account])

    -- Authorizing or rejecting follow requests
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "follow_requests"
    :> "authorize"
    :> ReqBody '[FormUrlEncoded] FollowRequestBody
    :> Post '[JSON] (Either Error ())
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "follow_requests"
    :> "reject"
    :> ReqBody '[FormUrlEncoded] FollowRequestBody
    :> Post '[JSON] (Either Error ())

  -- Follows

    -- Following a remote user
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "follows"
    :> ReqBody '[FormUrlEncoded] FollowBody
    :> Post '[JSON] (Either Error ())
 
  -- Instances

    -- Getting instance information
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "instance"
    :> Get '[JSON] (Either Error Instance)

  -- Media

    -- Uploading a media attachment
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "media"
    :> Post '[JSON] (Either Error Attachment) -- TODO: form data

  -- Mutes

    -- Fetching a user's mutes
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "mutes"
    :> Get '[JSON] (Either Error [Account])

  -- Notifications

    -- Fetching a user's notifications
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "notifications"
    :> Get '[JSON] (Either Error [Notification])
    -- Getting a single notification
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "notifications"
    :> Capture "id" Int
    :> Get '[JSON] (Either Error Notification)
    -- Clearing notifications
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "notifications"
    :> "clear"
    :> Post '[JSON] (Either Error ())

  -- Reports

    -- Fetching a user's reports
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "reports"
    :> Get '[JSON] (Either Error [Report])
    -- Reporting a user
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "reports"
    :> ReqBody '[FormUrlEncoded] ReportBody
    :> Post '[JSON] (Either Error Report)

  -- Search

    -- Searching for content
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "search"
    :> QueryParam "q" T.Text
    :> QueryParam "resolve" Bool
    :> Get '[JSON] (Either Error Results)

  -- Statuses

    -- Fetching a status
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> Capture "id" Int
    :> Get '[JSON] (Either Error Status)
    -- Getting status context
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> Capture "id" Int
    :> "context"
    :> Get '[JSON] (Either Error Context)
    -- Getting a card associated with a status
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> Capture "id" Int
    :> "card"
    :> Get '[JSON] (Either Error Card)
    -- Getting who reblogged/favourited a status
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> Capture "id" Int
    :> "reblogged_by"
    :> Get '[JSON] (Either Error [Account])
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> Capture "id" Int
    :> "favourited_by"
    :> Get '[JSON] (Either Error [Account])
    -- Posting a new status
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> ReqBody '[FormUrlEncoded] NewStatusBody
    :> Post '[JSON] (Either Error Status)
    -- Deleting a status
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> Capture "id" Int
    :> Delete '[JSON] (Either Error ())
    -- Reblogging/unreblogging a status
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> Capture "id" Int
    :> "reblog"
    :> Post '[JSON] (Either Error Status)
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> Capture "id" Int
    :> "unreblog"
    :> Post '[JSON] (Either Error Status)
    -- Favouriting/unfavouriting a status
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> Capture "id" Int
    :> "favourite"
    :> Post '[JSON] (Either Error Status)
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "statuses"
    :> Capture "id" Int
    :> "unfavourite"
    :> Post '[JSON] (Either Error Status)

  -- Timelines

    -- Retrieving a timeline
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "timeline"
    :> "home"
    :> Get '[JSON] (Either Error [Status])
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "timeline"
    :> "public"
    :> QueryParam "local" Bool
    :> Get '[JSON] (Either Error [Status])
  :<|> Header "Authorization" T.Text
    :> "api" :> "v1"
    :> "timeline"
    :> "tag"
    :> Capture "hashtag" T.Text
    :> QueryParam "local" Bool
    :> Get '[JSON] (Either Error [Status])

-- JSON objects used in the API

data Account = Account
    -- The ID of the account
    { id              :: Int
    -- The username of the account
    , username        :: T.Text
    -- Equals username for local users, includes @domain for remote ones
    , acct            :: T.Text
    -- The account's display name
    , display_name    :: T.Text
    -- Biography of user
    , note            :: T.Text
    -- URL of the user's profile page (can be remote)
    , url             :: T.Text
    -- URL to the avatar image
    , avatar          :: T.Text
    -- URL to the header image
    , header          :: T.Text
    -- Boolean for if account can be followed without waiting for approval
    , locked          :: Bool
    -- The time the account was created
    , created_at      :: T.Text
    -- The number of followers for the account
    , followers_count :: Int
    -- The number of accounts the given account is following
    , following_count :: Int
    -- The number of statuses the account has made
    , statuses_count  :: Int
    } deriving(Show, Generic)
instance ToJSON Account where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Account

data Application = Application
    -- Name of the app
    { name    :: T.Text
    -- Homepage URL of the app
    , website :: T.Text
    } deriving(Show, Generic)
instance ToJSON Application where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Application

data Attachment = Attachment
    -- ID of the attachment
    { id          :: Int
    -- One of: "image", "video", "gifv"
    --, type_ :: T.Text
    -- URL of the locally hosted version of the image
    , url         :: T.Text
    -- For remote images, the remote URL of the original image
    , remote_url  :: T.Text
    -- URL of the preview image
    , preview_url :: T.Text
    -- Shorter URL for the image, for insertion into text
    -- (only present on local images)
    , text_url    :: T.Text
    } deriving(Show, Generic)
instance ToJSON Attachment where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Attachment

data Card = Card
    -- The url associated with the card
    { url         :: T.Text
    -- The title of the card
    , title       :: T.Text
    -- The card description
    , description :: T.Text
    -- The image associated with the card, if any
    , image       :: T.Text
    } deriving(Show, Generic)
instance ToJSON Card where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Card

data Context = Context
    -- The ancestors of the status in the conversation
    { ancestors   :: [Status]
    -- The descendants of the status in the conversation
    , descendants :: [Status]
    } deriving(Show, Generic)
instance ToJSON Context where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Context

data Error = Error
    { error :: T.Text
    } deriving(Show, Generic)
instance ToJSON Error where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Error

data Instance = Instance
    -- URI of the current instance
    { uri         :: T.Text
    -- The instance's title
    , title       :: T.Text
    -- A description for the instance
    , description :: T.Text
    -- An email address which can be used to contact the instance administrator
    , email       :: T.Text
    } deriving(Show, Generic)
instance ToJSON Instance where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Instance

data Mention = Mention
    -- URL of user's profile (can be remote)
    { url      :: T.Text
    -- The username of the account
    , username :: T.Text
    -- Equals username for local users, includes @domain for remote ones
    , acct     :: T.Text
    -- Account ID
    , id       :: Int
    } deriving(Show, Generic)
instance ToJSON Mention where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Mention

data Notification = Notification
    -- The notification ID
    { id         :: Int
    -- One of: "mention", "reblog", "favourite", "follow"
    --, type_      :: T.Text
    -- The time the notification was created
    , created_at :: T.Text
    -- The Account sending the notification to the user
    , account    :: Account
    -- The Status associated with the notification, if applicible
    , status     :: Maybe Status
    } deriving(Show, Generic)
instance ToJSON Notification where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Notification

data Relationship = Relationship
    -- Whether the user is currently following the account
    { following   :: Bool
    -- Whether the user is currently being followed by the account
    , followed_by :: Bool
    -- Whether the user is currently blocking the account
    , blocking    :: Bool
    -- Whether the user is currently muting the account
    , muting      :: Bool
    -- Whether the user has requested to follow the account
    , requested   :: Bool
    } deriving(Show, Generic)
instance ToJSON Relationship where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Relationship

data Report = Report
    -- The ID of the report
    { id           :: Int
    -- The action taken in response to the report
    , action_taken :: T.Text
    } deriving(Show, Generic)
instance ToJSON Report where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Report

data Results = Results
    -- An array of matched Accounts
    { accounts :: [Account]
    -- An array of matchhed Statuses
    , statuses :: [Status]
    -- An array of matched hashtags, as strings
    , hashtags :: [T.Text]
    } deriving(Show, Generic)
instance ToJSON Results where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Results

data Status = Status
    -- The ID of the status
    { id                     :: Int
    -- A Fediverse-unique resource ID
    , uri                    :: T.Text
    -- URL to the status page (can be remote)
    , url                    :: T.Text
    -- The Account which posted the status
    , account                :: Account
    -- null or the ID of the status it replies to
    , in_reply_to_id         :: Maybe Int
    -- null or the ID of the account it replies to
    , in_reply_to_account_id :: Maybe Int
    -- null or the reblogged Status
    , reblog                 :: Maybe Status
    -- Body of the status; this will contain HTML
    -- (remote HTML already sanitized)
    , content                :: T.Text
    -- The time the status was created
    , created_at             :: T.Text
    -- The number of reblogs for the status
    , reblogs_count          :: Int
    -- The number of favourites for the status
    , favourites_count       :: Int
    -- Whether the authenticated user has reblogged the status
    , reblogged              :: Bool
    -- Whether the authenticated user has favourited the status
    , favourited             :: Bool
    -- Whether media attachments should be hidden by default
    , sensitive              :: Bool
    -- If not empty, warning text that should be
    -- displayed before the actual content
    , spoiler_text           :: T.Text
    -- One of: public, unlisted, private, direct
    , visibility             :: T.Text
    -- An array of Attachments
    , media_attachments      :: [Attachment]
    -- An array of Mentions
    , mentions               :: [Mention]
    -- An array of Tags
    , tags                   :: [Tag]
    -- Application from which the status was posted
    , application            :: Application
    } deriving(Show, Generic)
instance ToJSON Status where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Status

data Tag = Tag
    -- The hashtag, not including the preceding #
    { name :: T.Text
    -- The URL of the hashtag
    , url  :: T.Text
    } deriving(Show, Generic)
instance ToJSON Tag where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Tag

-- Form objects used in the API

data AppRegisterBody = AppRegisterBody
    -- Name of your application
    { client_name   :: T.Text
    -- Where the user should be redirected after authorization
    -- (for no redirect, use urn:ietf:wg:oauth:2.0:oob)
    , redirect_uris :: T.Text
    -- This can be a space-separated list of the following items:
    -- "read", "write" and "follow"
    , scopes        :: T.Text
    -- (optional) URL to the homepage of your app
    , website       :: T.Text
    } deriving(Show, Generic)
instance ToForm AppRegisterBody

data FollowRequestBody = FollowRequestBody
    -- The id of the account to authorize or reject
    { id :: T.Text
    } deriving(Show, Generic)
instance ToForm FollowRequestBody

data FollowBody = FollowBody
    -- username@domain of the person you want to follow
    { uri :: T.Text
    } deriving(Show, Generic)
instance ToForm FollowBody

data ReportBody = ReportBody
    -- The ID of the account to report
    { account_id :: Int
    -- The IDs of statuses to report
    , status_ids :: [Int]
    -- A comment to associate with the report
    , comment    :: T.Text
    } deriving(Show, Generic)
instance ToForm ReportBody

data NewStatusBody = NewStatusBody
    -- The text of the status
    { status         :: T.Text
    -- local ID of the status you want to reply to
    , in_reply_to_id :: Maybe Int
    -- array of media IDs to attach to the status (maximum 4)
    , media_ids      :: Maybe Int -- TODO: make list (wtf?)
    -- set this to mark the media of the status as NSFW
    , sensitive      :: Maybe Bool
    -- text to be shown as a warning before the actual content
    , spoiler_text   :: Maybe T.Text
    -- either "direct", "private", "unlisted" or "public"
    , visibility     :: Maybe T.Text
    } deriving(Show, Generic)
instance ToForm NewStatusBody

data AppInfo = AppInfo
    { app_id        :: T.Text
    , client_id     :: T.Text
    , client_secret :: T.Text
    , server        :: T.Text
    , app_name      :: T.Text
    } deriving(Show, Generic)

instance ToJSON   AppInfo
instance FromJSON AppInfo

data OAuthCode = OAuthCode { oauth_code :: T.Text } deriving(Show, Generic)
instance ToJSON   OAuthCode
instance FromJSON OAuthCode

-- {"id":16703,"redirect_uri":"urn:ietf:wg:oauth:2.0:oob","client_id":"47a9437c2979549e9d155a95cfc5befb8187780325d9f8908d351c47c6338d07","client_secret":"a3b6aea59a6aedbb76e0f41dfbc32a0b41ecdcabcbc3d8fe1ddabd7faa6be4cb"}
data AppRegistration = AppRegistration
    { id :: T.Text
    , client_id :: T.Text
    , client_secret :: T.Text
    , redirect_uri :: T.Text
    } deriving(Show, Generic)
instance ToJSON   AppRegistration
instance FromJSON AppRegistration

appRegToAppInfo :: T.Text -> AppRegistration -> AppInfo
appRegToAppInfo server (AppRegistration i c s _) = AppInfo i c s server "TODO: replace me"

getAccount          :: Maybe T.Text -> AccountID -> ClientM (Either Error Account)
getCurrentUser      :: Maybe T.Text -> ClientM (Either Error Account)
getFollowers        :: Maybe T.Text -> AccountID -> ClientM (Either Error [Account])
getFollowing        :: Maybe T.Text -> AccountID -> ClientM (Either Error [Account])
getStatuses         :: Maybe T.Text -> AccountID -> Maybe Bool -> Maybe Bool -> ClientM (Either Error [Status])
followAccount       :: Maybe T.Text -> AccountID -> ClientM (Either Error Account)
unfollowAccount     :: Maybe T.Text -> AccountID -> ClientM (Either Error Account)
blockAccount        :: Maybe T.Text -> AccountID -> ClientM (Either Error Account)
unblockAccount      :: Maybe T.Text -> AccountID -> ClientM (Either Error Account)
muteAccount         :: Maybe T.Text -> AccountID -> ClientM (Either Error Account)
unmuteAccount       :: Maybe T.Text -> AccountID -> ClientM (Either Error Account)
getRelationships    :: Maybe T.Text -> ClientM (Either Error [Relationship])
searchAccounts      :: Maybe T.Text -> Maybe T.Text -> Maybe Int -> ClientM (Either Error [Account])
registerApp         :: AppRegisterBody -> ClientM (Either Error AppRegistration)
getBlocks           :: Maybe T.Text -> ClientM (Either Error [Account])
getFavs             :: Maybe T.Text -> ClientM (Either Error [Status])
getFollowReqs       :: Maybe T.Text -> ClientM (Either Error [Account])
approveFollowReq    :: Maybe T.Text -> FollowRequestBody -> ClientM (Either Error ())
rejectFollowReq     :: Maybe T.Text -> FollowRequestBody -> ClientM (Either Error ())
followRemoteAccount :: Maybe T.Text -> FollowBody -> ClientM (Either Error ())
getInstanceInfo     :: Maybe T.Text -> ClientM (Either Error Instance)
uploadMedia         :: Maybe T.Text -> ClientM (Either Error Attachment) -- TODO: form data
getMutes            :: Maybe T.Text -> ClientM (Either Error [Account])
getNotifs           :: Maybe T.Text -> ClientM (Either Error [Notification])
getSingleNotif      :: Maybe T.Text -> Int -> ClientM (Either Error Notification)
clearNotifs         :: Maybe T.Text -> ClientM (Either Error ())
getReports          :: Maybe T.Text -> ClientM (Either Error [Report])
makeReport          :: Maybe T.Text -> ReportBody -> ClientM (Either Error Report)
search              :: Maybe T.Text -> Maybe T.Text -> Maybe Bool -> ClientM (Either Error Results)
getStatus           :: Maybe T.Text -> Int -> ClientM (Either Error Status)
getStatusContext    :: Maybe T.Text -> Int -> ClientM (Either Error Context)
getStatusCard       :: Maybe T.Text -> Int -> ClientM (Either Error Card)
getStatusBoosters   :: Maybe T.Text -> Int -> ClientM (Either Error [Account])
getStatusFavers     :: Maybe T.Text -> Int -> ClientM (Either Error [Account])
makeNewStatus       :: Maybe T.Text -> NewStatusBody -> ClientM (Either Error Status)
deleteStatus        :: Maybe T.Text -> Int -> ClientM (Either Error ())
boostStatus         :: Maybe T.Text -> Int -> ClientM (Either Error Status)
unBoostStatus       :: Maybe T.Text -> Int -> ClientM (Either Error Status)
favStatus           :: Maybe T.Text -> Int -> ClientM (Either Error Status)
unFavStatus         :: Maybe T.Text -> Int -> ClientM (Either Error Status)
getTimeline         :: Maybe T.Text -> ClientM (Either Error [Status])
getPubTimeline      :: Maybe T.Text -> Maybe Bool -> ClientM (Either Error [Status])
getTagTimeline      :: Maybe T.Text -> T.Text -> Maybe Bool -> ClientM (Either Error [Status])

api :: S.Proxy MastodonAPI
api = S.Proxy

getAccount
  :<|> getCurrentUser
  :<|> getFollowers
  :<|> getFollowing
  :<|> getStatuses
  :<|> followAccount
  :<|> unfollowAccount
  :<|> blockAccount
  :<|> unblockAccount
  :<|> muteAccount
  :<|> unmuteAccount
  :<|> getRelationships
  :<|> searchAccounts
  :<|> registerApp
  :<|> getBlocks
  :<|> getFavs
  :<|> getFollowReqs
  :<|> approveFollowReq
  :<|> rejectFollowReq
  :<|> followRemoteAccount
  :<|> getInstanceInfo
  :<|> uploadMedia
  :<|> getMutes
  :<|> getNotifs
  :<|> getSingleNotif
  :<|> clearNotifs
  :<|> getReports
  :<|> makeReport
  :<|> search
  :<|> getStatus
  :<|> getStatusContext
  :<|> getStatusCard
  :<|> getStatusBoosters
  :<|> getStatusFavers
  :<|> makeNewStatus
  :<|> deleteStatus
  :<|> boostStatus
  :<|> unBoostStatus
  :<|> favStatus
  :<|> unFavStatus
  :<|> getTimeline
  :<|> getPubTimeline
  :<|> getTagTimeline
    = client api
