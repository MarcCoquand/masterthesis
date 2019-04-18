module Main where

import           Kudos              (Load, Write)
import qualified Kudos
import qualified Response
import           System.Environment (getEnv)
import qualified Web.Scotty         as Web


router :: (Write, Load) -> Web.ScottyM ()
router handlers =
    do  Web.post "/slack/addkudo" (Response.handler handlers)
        Web.post "/slack/listall" (Response.handler handlers)
        Web.post "/slack/listspecific" (Response.handler handlers)


main :: IO ()
main =
    do  mongoConnection <- getEnv "MONGODB_URI"
        stringPort <-getEnv "PORT"
        let port =
                (read stringPort :: Int)
        handlers <- Kudos.effectful mongoConnection
        Web.scotty port (router handlers)
