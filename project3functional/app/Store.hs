module Store where

import           Data.IORef      (modifyIORef', newIORef, readIORef)
import           Data.Vault.Lazy (Vault)
import qualified Data.Vault.Lazy as Vault


data Store m userId kudos =
    MakeStore
    { add          :: kudos -> m ()
    , getTenLatest :: m [kudos]
    , find         :: userId -> m [kudos]
    , count        :: userId -> m Int
    }


mock
    :: [kudos]
    -> (userId -> [kudos] -> [kudos])
    -> (userId -> [kudos] -> Int)
    -> IO (Store IO userId kudos)
mock kudosList findFunction countFunction =
    do  ref <- newIORef kudosList
        return (MakeStore
            { add =
                \kudos ->
                    modifyIORef' ref (\list -> list ++ [kudos])
            , getTenLatest =
                do  list <- readIORef ref
                    return (take 10 list)
            , find =
                \userId ->
                    do  list <- readIORef ref
                        return (findFunction userId list)
            , count =
                \userId ->
                    do  list <- readIORef ref
                        return (countFunction userId list)
            })



