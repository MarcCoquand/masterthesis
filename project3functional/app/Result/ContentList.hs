module Result.ContentList where


import           Data.Aeson   (FromJSON, ToJSON)
import qualified Data.Aeson   as Json
import           Data.Text    (Text)
import           GHC.Generics (Generic)

type ContentList =
    [Content]

data Content =
    MakeText
    { text :: Text
    }
    deriving (Generic, Show)

instance ToJSON Content where
instance FromJSON Content where

make :: [Text] -> ContentList
make =
    map MakeText
