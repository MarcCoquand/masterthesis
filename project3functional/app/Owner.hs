{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Owner where

import qualified Data.Bson                       as Database
import           Data.Maybe                      (isJust)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Text.Read                  (Reader)
import qualified Data.Text.Read                  as Text.Read
import           Data.Typeable                   (Typeable)
import qualified Database.MongoDB                as Database
import qualified Test.QuickCheck                 as Quickcheck
import           Test.QuickCheck.Arbitrary       (Arbitrary, arbitrary)
import qualified Test.QuickCheck.Gen             as Quickcheck
import           Text.ParserCombinators.ReadP    (ReadP)
import qualified Text.ParserCombinators.ReadP    as Parse
import qualified Text.ParserCombinators.ReadPrec as Parse
import           Text.Read                       (readMaybe, readPrec)


newtype Owner = UnsafeOwner {unOwner :: Text}
    deriving (Eq, Show, Typeable, Database.Val)


instance Read Owner where
    readPrec =
        Parse.lift reader


instance Arbitrary Owner where
    arbitrary =
        do  withParam <- Quickcheck.choose (True, False)
            code <-
                do  string <-
                        Quickcheck.suchThat arbitrary (all isNumberOrCharacter)
                    return . take 8 $ string
            if withParam then
                do  username <-
                        Quickcheck.suchThat
                            arbitrary
                            (all isNumberOrCharacter)


                    return
                        . UnsafeOwner
                        . Text.pack
                        $ "<@" ++ code ++ "|" ++ username ++ ">"
            else
                return . UnsafeOwner . Text.pack $ "<@" ++ code ++ ">"


toText :: Owner -> Text
toText (UnsafeOwner owner) =
    owner


isNumberOrCharacter :: Char -> Bool
isNumberOrCharacter c =
    elem c $
        ['a'..'z'] ++
        ['A'..'Z'] ++
        ['0'..'9']


containsSep :: ReadP String
containsSep =
    do  sep <- Parse.char '|'
        i2 <- Parse.munch isNumberOrCharacter
        return ([sep] ++ i2)


-- | Parses a slack user in the format: <@ane13|name> or <@nasrt3>
reader :: ReadP Owner
reader =
    do  s <- Parse.char '<'
        at <- Parse.char '@'
        i <- Parse.munch1 isNumberOrCharacter
        opt <- Parse.option [] containsSep
        e <- Parse.char '>'
        return (UnsafeOwner $ Text.pack ([s] ++ [at] ++ i ++ opt ++ [e]))


parse :: Text -> Maybe Owner
parse =
    readMaybe . Text.unpack

