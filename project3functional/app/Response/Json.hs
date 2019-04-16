{- To get around global namespacing in Haskell we can put them in modules
 -}
module Response.Json where

import           Data.Aeson    (FromJSON, ToJSON)
import qualified Data.Aeson    as Json
import           Data.Text     (Text)
import           GHC.Generics  (Generic)
import qualified Response.Text as Response


data Response =
    Response
    { respones_type :: Text
    , text          :: Text
    , attachments   :: [Response.Text]
    }
    deriving (Generic, Show)

instance ToJSON Response where
instance FromJSON Response where
