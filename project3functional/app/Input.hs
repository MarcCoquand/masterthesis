{-# LANGUAGE PatternSynonyms #-}
module Input where


import           Data.Text (Text)
import           Kudos     (Kudos)
import qualified Kudos
import           Test.QuickCheck           (Gen)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Owner


newtype Input = UnsafeInput {unInput :: Text}


instance Arbitrary Input where 
    arbitrary = 
        do  anything <- (arbitrary :: Gen String) 
            owner <- (arbitrary :: Gen Owner)


make :: Text -> Input
make =
    UnsafeInput


data Invalid
    = UserDoesNotExist
    | Format


data Valid
    = GiveKudos Kudos
    | WantSpecificKudos Kudos.Owner
    | WantRanking


showMistake :: Invalid -> Text
showMistake mistake =
    case mistake of
        UserDoesNotExist ->
            "This user either does not exist or has not received any kudos!"
        Format ->
            "Command must be of format \"/kudos @user thanks!\""


parse :: Input -> Either Invalid Valid
parse =
    undefined

