module Data.Set.Extra where

import qualified Control.Monad as List (filterM)
import           Data.Set      (Set)
import qualified Data.Set      as Set

filterM :: (Ord a, Monad m) => (a -> m Bool) -> Set a -> m (Set a)
filterM p s =
    List.filterM p (Set.toList s) >>= return . Set.fromList

catMaybes sm =
    Set.fold (\ mx s -> maybe s (`Set.insert` s) mx) Set.empty sm

