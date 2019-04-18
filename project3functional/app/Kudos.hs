module Kudos where

import           Data.Bson                 ((!?), (=:))
import qualified Data.Bson
import           Data.Function             ((&))
import           Data.HashMap.Lazy         (HashMap)
import qualified Data.HashMap.Lazy         as HashMap
import           Data.IORef                (modifyIORef', newIORef, readIORef)
import           Data.Maybe                (catMaybes)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Time.Clock           (UTCTime)
import qualified Data.Time.Clock           as Time
import           Data.Time.Clock.POSIX     (POSIXTime, utcTimeToPOSIXSeconds)
import qualified Database.MongoDB          as Database
import qualified Database.MongoDB.Query    as Query
import           Test.QuickCheck           (Gen)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)



data Write  =
    MakeWrite
    { add :: Kudos -> IO ()
    }


data Load =
    MakeLoad
    { getTenLatest :: IO [Kudos]
    , specific     :: Owner -> IO [Kudos]
    , count        :: Owner -> IO Int
    }


type Message =
    Text


type Owner =
    Text


data Kudos =
    MakeKudos
    { message :: Message
    , owner   :: Owner
    }


instance Arbitrary Kudos where
    arbitrary =
        do  msg <- (arbitrary :: Gen String)
            own <- (arbitrary :: Gen String)
            return (MakeKudos (Text.pack msg) (Text.pack own))


make :: Message -> Owner -> Kudos
make =
    MakeKudos


show :: Kudos -> Text
show kudos =
    "* Kudos to: " <> (owner kudos) <> ", message: " <> (message kudos)



-- * SYMBOLIC


isOwner :: Owner -> Kudos -> Bool
isOwner user kudos =
    owner kudos == user


takeTenOwned :: Owner -> [Kudos] -> [Kudos]
takeTenOwned user list =
    take 10 (filter (isOwner user) list)


countSymbolic :: Owner -> [Kudos] -> Int
countSymbolic user list =
    length (filter (isOwner user) list)


addToList ::  Kudos -> [Kudos] -> [Kudos]
addToList kudos list =
    list ++ [kudos]


mock :: [Kudos] -> IO (Load, Write)
mock kudosList =
    do  ref <- newIORef kudosList
        return (MakeLoad
            {  getTenLatest =
                do  list <- readIORef ref
                    return (take 10 list)
            , specific =
                \userId ->
                    do  list <- readIORef ref
                        return (takeTenOwned userId list)
            , count =
                \userId ->
                    do  list <- readIORef ref
                        return (countSymbolic userId list)
            },
            MakeWrite
                {add =
                    \kudos ->
                        modifyIORef' ref (addToList kudos)
                })



-- * SERIALIZATION


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



-- * EFFECTFUL USING MONGODB


countQuery :: Owner -> Database.Action IO Int
countQuery userId =
    do  ref <- Database.find
            (Database.select ["owner" =: userId ] "kudos")
            { Database.limit = 10
            , Database.sort = sortByTimestamp
            }
        documents <- Database.rest ref
        return (length documents)


specificQuery :: Owner -> Database.Action IO [Kudos]
specificQuery userId =
    do  ref <- Database.find
            (Database.select ["owner" =: userId ] "kudos")
            { Database.limit = 10
            , Database.sort = sortByTimestamp
            }
        documents <- Database.rest ref
        return (fromBsonList documents)


addQuery :: POSIXTime -> Kudos -> Query.Action IO ()
addQuery timestamp kudos =
        do  withTimestamp timestamp (toBson kudos)
                & Database.insert "kudos"
            return ()


tenLatestQuery :: Database.Action IO [Kudos]
tenLatestQuery =
    do  ref <- Database.find
            (Database.select [] "kudos")
            { Database.limit = 10
            , Database.sort = sortByTimestamp
            }
        documents <- Database.rest ref
        return (fromBsonList documents)


withTimestamp :: POSIXTime -> Database.Document -> Database.Document
withTimestamp timestamp value =
        value ++ [ "timestamp" =: timestamp]


sortByTimestamp :: Database.Document
sortByTimestamp =
    [("timestamp" :: Text) =: (1 :: Integer)]


effectful :: String -> IO (Write, Load)
effectful connection =
    do  pipe <- Database.connect (Database.host connection)
        return (MakeWrite
            { add =
                \kudos ->
                    do  utcTime <- Time.getCurrentTime
                        let timestamp =
                                utcTimeToPOSIXSeconds utcTime
                        runDatabase pipe
                            (addQuery timestamp kudos)
            }, MakeLoad
                { getTenLatest =
                    runDatabase pipe tenLatestQuery
                , specific =
                    runDatabase pipe . specificQuery
                , count =
                    runDatabase pipe . countQuery
                })


runDatabase :: Database.Pipe -> Query.Action IO a -> IO a
runDatabase pipe action =
    Database.access
        pipe
        Query.master
        "kudos"
        action

