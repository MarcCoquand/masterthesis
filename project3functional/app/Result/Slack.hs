{- To get around global namespacing in Haskell we can put them in modules
 -}
module Result.Slack where

import           Data.Aeson         (FromJSON, ToJSON)
import qualified Data.Aeson         as Json
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           Result.ContentList (ContentList)
import qualified Result.ContentList as ContentList


data Response =
    Response
    { respones_type :: Text
    , text          :: Text
    , attachments   :: ContentList
    }
    deriving (Generic, Show)


instance ToJSON Response where
instance FromJSON Response where


channelResponse :: Text -> [Text] -> Json.Value
channelResponse header list =
    Json.toJSON (Response "in_channel" header (ContentList.make list))
