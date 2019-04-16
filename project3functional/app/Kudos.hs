module Kudos where

import           Data.Bson              ((!?), (=:))
import qualified Data.Bson
import           Data.Function          ((&))
import           Data.HashMap.Lazy      (HashMap)
import qualified Data.HashMap.Lazy      as HashMap
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import qualified Data.Time.Clock        as Time
import           Data.Time.Clock.POSIX  (POSIXTime, utcTimeToPOSIXSeconds)
import qualified Database.MongoDB       as Database
import qualified Database.MongoDB.Query as Query
import           Database.Persist       (Key)
import           Store                  (Store)
import qualified Store



type Message =
    Text


type Owner =
    Text


data Kudos =
    MakeKudos
    {  message :: Message
    , owner    :: Owner
    }


update ::  Kudos -> Maybe Message -> Kudos
update kudos maybeMessage  =
    case maybeMessage of
        Just msg ->
            MakeKudos
                {  message = msg
                , owner = (owner kudos)
                }
        Nothing ->
            MakeKudos
                { message = ""
                , owner = (owner kudos)
                }



isOwner :: Owner -> Kudos -> Bool
isOwner user kudos =
    owner kudos == user


takeTenOwned :: Owner -> [Kudos] -> [Kudos]
takeTenOwned user list =
    take 10 (filter (isOwner user) list)


count :: Owner -> [Kudos] -> Int
count user list =
    length (filter (isOwner user) list)


mock :: [Kudos] -> IO (Store IO Owner Kudos)
mock kudosList =
    Store.mock kudosList takeTenOwned count


toBson :: Kudos -> Database.Document
toBson kudos =
    [ "message" =: (message kudos)
    , "owner" =: (owner kudos)
    ]

fromBson :: Database.Document -> Maybe Kudos
fromBson document =
    do  m <- document !? "message" :: Maybe Text
        o <- document !? "owner" :: Maybe Text
        return (MakeKudos {owner = m, message = m})

fromBsonList :: [Database.Document] -> [Kudos]
fromBsonList documents =
    catMaybes (map fromBson documents)

withTimestamp :: POSIXTime -> Database.Document -> Database.Document
withTimestamp timestamp value =
        value ++ [ "timestamp" =: timestamp]


addQuery :: POSIXTime -> Kudos -> Query.Action IO ()
addQuery timestamp kudos =
        do  withTimestamp timestamp (toBson kudos)
                & Database.insert "kudos"
            return ()

sortByTimestamp :: Database.Document
sortByTimestamp =
    [("timestamp" :: Text) =: (1 :: Integer)]


tenLatestQuery :: Database.Action IO [Kudos]
tenLatestQuery =
    do  ref <- Database.find
            (Database.select [] "kudos")
            { Database.limit = 10
            , Database.sort = sortByTimestamp
            }
        documents <- Database.rest ref
        return (fromBsonList documents)


findQuery :: Owner -> Database.Action IO [Kudos]
findQuery userId =
    do  ref <- Database.find
            (Database.select ["owner" =: userId ] "kudos")
            { Database.limit = 10
            , Database.sort = sortByTimestamp
            }
        documents <- Database.rest ref
        return (fromBsonList documents)


countQuery :: Owner -> Database.Action IO Int
countQuery userId =
    do  ref <- Database.find
            (Database.select ["owner" =: userId ] "kudos")
            { Database.limit = 10
            , Database.sort = sortByTimestamp
            }
        documents <- Database.rest ref
        return (length documents)


runDatabase :: Database.Pipe -> Query.Action IO a -> IO a
runDatabase pipe action =
    Database.access
        pipe
        Query.master
        "kudos"
        action


effectful :: String -> IO (Store IO Owner Kudos)
effectful connection =
    do  pipe <- Database.connect (Database.host "127.0.0.1")
        return (Store.MakeStore
            { Store.add =
                \kudos ->
                    do  utcTime <- Time.getCurrentTime
                        let timestamp =
                                utcTimeToPOSIXSeconds utcTime
                        runDatabase pipe
                            (addQuery timestamp kudos)
            , Store.getTenLatest =
                runDatabase pipe tenLatestQuery
            , Store.find =
                runDatabase pipe . findQuery
            , Store.count =
                runDatabase pipe . countQuery
            })

