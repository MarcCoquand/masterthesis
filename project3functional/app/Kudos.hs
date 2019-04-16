module Kudos where

import           Data.Bson              ((=:))
import qualified Data.Bson
import           Data.Function          ((&))
import           Data.HashMap.Lazy      (HashMap)
import qualified Data.HashMap.Lazy      as HashMap
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import qualified Data.Time.Clock        as Time
import           Data.Time.Clock.POSIX  (POSIXTime, utcTimeToPOSIXSeconds)
import qualified Database.MongoDB       as Database
import qualified Database.MongoDB.Query as Database.Query
import           Database.Persist       (Key)
import           Store                  (Store)
import qualified Store



type Message =
    Text


type Owner =
    Text


data Kudos =
    MakeKudos
    { points  :: Int
    , message :: Message
    , owner   :: Owner
    }


update ::  Kudos -> Maybe Message -> Kudos
update kudos maybeMessage  =
    case maybeMessage of
        Just msg ->
            MakeKudos
                { points = (points kudos) + 1
                , message = msg
                , owner = (owner kudos)
                }
        Nothing ->
            MakeKudos
                { points = (points kudos) + 1
                , message = ""
                , owner = (owner kudos)
                }



isOwner :: Owner -> Kudos -> Bool
isOwner user kudos =
    owner kudos == user


takeTenOwned user list =
    take 10 (filter (isOwner user) list)


mock :: [Kudos] -> IO (Store IO Owner Kudos)
mock kudosList =
    Store.mock kudosList takeTenOwned


toBson :: Kudos -> Database.Document
toBson kudos =
    [ "points" =: (points kudos)
    , "message" =: (message kudos)
    , "owner" =: (owner kudos)
    ]


withTimestamp :: POSIXTime -> Database.Document -> Database.Document
withTimestamp timestamp value =
        value ++ [ "timestamp" =: timestamp]


addEffectful :: POSIXTime -> Kudos -> Database.Query.Action IO ()
addEffectful timestamp kudos =
        do  withTimestamp timestamp (toBson kudos)
                & Database.insert "kudos"
            return ()


tenLatestEffectful =
    Database.find


effectful :: String -> IO (Store IO Owner Kudos)
effectful connection =
    do  pipe <- Database.connect (Database.host "127.0.0.1")
        return (Store.MakeStore
            { Store.add =
                \kudos ->
                    do  utcTime <- Time.getCurrentTime
                        let timestamp =
                                utcTimeToPOSIXSeconds utcTime
                        Database.access
                            pipe
                            Database.Query.master
                            "kudos"
                            (addEffectful timestamp kudos)
            , Store.getTenLatest =
                tenLatestEffectful

            })

