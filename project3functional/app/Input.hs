{-# LANGUAGE PatternSynonyms #-}
module Input where


import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Kudos                           (Kudos)
import qualified Kudos
import           Owner                           (Owner)
import qualified Owner
import           Test.QuickCheck                 (Gen)
import           Test.QuickCheck.Arbitrary       (Arbitrary, arbitrary)
import           Text.ParserCombinators.ReadP    (ReadP)
import qualified Text.ParserCombinators.ReadP    as Parse
import qualified Text.ParserCombinators.ReadPrec as Parse hiding (choice)
import           Text.Read                       (readMaybe, readPrec)


newtype Input = UnsafeInput {unInput :: Text}


instance Arbitrary Input where
    arbitrary =
        do  anything <- (arbitrary :: Gen String)
            owner <- (arbitrary :: Gen Owner)
            return . UnsafeInput $
                (Owner.toText owner <> " " <> (Text.pack anything))


instance Read Valid where
    readPrec =
        Parse.lift reader


make :: Text -> Input
make =
    UnsafeInput


data Invalid
    = UserDoesNotExist
    | Format


data Valid
    = GiveKudos Kudos
    | WantSpecificKudos Owner
    | WantRanking


showMistake :: Invalid -> Text
showMistake mistake =
    case mistake of
        UserDoesNotExist ->
            "This user either does not exist or has not received any kudos!"
        Format ->
            "Command must be of format \"/kudos @user thanks!\""


parseGiveKudos :: ReadP Valid
parseGiveKudos =
    do  Parse.string "give"
        Parse.skipSpaces
        owner <- Owner.reader
        Parse.skipSpaces
        message <- Parse.manyTill (Parse.satisfy (\_ -> True)) Parse.eof
        return (GiveKudos (Kudos.make (Text.pack message) owner))


parseWantSpecificKudos :: ReadP Valid
parseWantSpecificKudos =
    do  Parse.string "list"
        Parse.skipSpaces
        owner <- Owner.reader
        return (WantSpecificKudos owner)


parseWantRanking :: ReadP Valid
parseWantRanking =
    do  Parse.string "list"
        return WantRanking


-- | Parses a slack user in the format: <@ane13|name> or <@nasrt3>
reader :: ReadP Valid
reader =
    Parse.choice [parseGiveKudos, parseWantSpecificKudos, parseWantRanking]



parse :: Input -> Either Invalid Valid
parse input =
    let
        result =
            readMaybe . Text.unpack . unInput $ input
    in
        case result of
            Just valid ->
                Right valid
            Nothing ->
                Left Format

