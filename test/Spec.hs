import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Codec.Test

main :: IO ()
main = hspec $ do
  describe "ham codec jt65" $ do
    prop "plainTextSplit" $ prop_plainTextSplit
    prop "plainText"      $ prop_plainText
    prop "callSign"       $ prop_callSign
    prop "block1"         $ prop_block1
    prop "locator"        $ prop_locator
    prop "message"        $ prop_message
    prop "roundTripMessage" $ prop_roundTripMessage

