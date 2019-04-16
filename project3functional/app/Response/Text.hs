module Response.Text where


import           Data.Aeson   (FromJSON, ToJSON)
import qualified Data.Aeson   as Json
import qualified Data.Text    as T
import           GHC.Generics (Generic)


data Text =
    MakeText
    { text :: T.Text
    }
    deriving (Generic, Show)

instance ToJSON Response.Text.Text where
instance FromJSON Response.Text.Text where
