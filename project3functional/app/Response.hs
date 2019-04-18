module Response where

import           Control.Monad ((<=<))
import qualified Data.Aeson    as Json
import           Data.Text     (Text)
import qualified Data.Text     as Text
import qualified Data.Text.IO  as Text
import           Input         (Input, Valid (..))
import qualified Input
import           Kudos         (Kudos)
import qualified Kudos
import           Result        (Result)
import qualified Result
import qualified Web.Scotty    as Web


data Response =
    Response
    { send :: Result -> IO Json.Value
    }


data Request =
    Request
    { getBody :: Input
    }


interpret :: (Kudos.Load, Kudos.Write) -> Input.Valid -> IO Result
interpret (loader, saver) input =
    case input of
        GiveKudos kudos ->
            do  Kudos.add saver kudos
                return (Result.Ok "Added todo" [])
        WantSpecificKudos owner ->
            do  items <- Kudos.specific loader owner
                return (Result.Ok "Current standing" (map Kudos.show items))
        WantRanking ->
            do  items <- Kudos.getTenLatest loader
                return (Result.Ok "Your latest kudos" (map Kudos.show items))


server :: (Kudos.Load,Kudos.Write, Request, Response) -> IO Json.Value
server (load, save, request, response) =
    let
        input =
            getBody request

        parsedInput =
            Input.parse input
    in
        case parsedInput of
            Right valid ->
                do  result <-
                        interpret (load, save) valid
                    send response result
            Left invalid ->
                send response . Result.Error . Input.showMistake $ invalid



-- * EFFECTFUL


slackResponse ::  Response
slackResponse =
    Response
        { send = return . Result.toSlackResponse }


handler :: (Kudos.Write, Kudos.Load) -> Web.ActionM ()
handler (writeDatabase,loadDatabase) =
    do  input <- Web.param "text"
        let request =
                Request { getBody = Input.make input }
        response <-
            Web.liftAndCatchIO
                (server (loadDatabase, writeDatabase, request, slackResponse))
        Web.json response



-- * SYMBOLIC


mockRequest :: Input -> Request
mockRequest input =
    Request
    { getBody =
        input
    }


responseHandler :: Response
responseHandler =
    Response
    { send =
        return . Json.toJSON
    }
