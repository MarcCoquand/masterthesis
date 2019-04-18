module Owner where

import           Data.Maybe                      (isJust)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Text.Read                  (Reader)
import qualified Data.Text.Read                  as Text.Read
import           Text.ParserCombinators.ReadP    (ReadP)
import qualified Text.ParserCombinators.ReadP    as Parse
import qualified Text.ParserCombinators.ReadPrec as Parse
import           Text.Read                       (readMaybe, readPrec)


newtype Owner = UnsafeOwner {unOwner :: Text}


instance Read Owner where
    readPrec =
        Parse.lift reader


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

