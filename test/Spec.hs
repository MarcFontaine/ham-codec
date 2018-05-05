import Test.Hspec
import Test.QuickCheck
import Codec.Test

main :: IO ()
main = hspec $ do
  describe "ham codec jt65" $ do
    it "prop_plainTextSplit" $ property prop_plainTextSplit
    it "prop_plainText"      $ property prop_plainText
    it "prop_callSign"       $ property prop_callSign
    it "prop_block1"         $ property prop_block1
    it "prop_locator"        $ property prop_locator
    it "prop_message"        $ property prop_message
    it "prop_roundTripMessage" $ property prop_roundTripMessage

