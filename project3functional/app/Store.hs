module Store where

import           Data.IORef      (modifyIORef', newIORef, readIORef)
import           Data.Vault.Lazy (Vault)
import qualified Data.Vault.Lazy as Vault


data Store m userId kudos =
    MakeStore
    { add          :: kudos -> m ()
    , getTenLatest :: m [kudos]
    , find         :: userId -> m [kudos]
    }


mock :: [kudos] -> (userId -> [kudos] -> [kudos]) -> IO (Store IO userId kudos)
mock kudosList findFunction =
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
            })



