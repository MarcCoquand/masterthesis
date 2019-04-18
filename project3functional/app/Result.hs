{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Result where


import qualified Data.Aeson    as Json
import           Data.Data     (Data)
import           Data.Text     (Text)
import qualified Data.Text     as Text
import           Data.Typeable (Typeable)
import           GHC.Generics
import           Kudos         (Kudos)
import qualified Result.Slack  as Slack


type Header =
    Text


type Content =
    [Text]


data Result
    = Ok Header Content
    | Error Header
    deriving (Data, Typeable, Show, Generic)


instance Json.ToJSON Result
instance Json.FromJSON Result


toSlackResponse :: Result -> Json.Value
toSlackResponse result =
    case result of
        Ok header content ->
            Slack.channelResponse header content
        Error error ->
            Slack.channelResponse ("Invalid input: " <> error) []


show :: Result -> Text
show result =
    case result of
        Ok header content ->
            header <> Text.unlines content
        Error header ->
            "Error:" <> header
