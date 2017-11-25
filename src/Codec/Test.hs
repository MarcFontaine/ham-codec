{-# Language TemplateHaskell #-}
module Codec.Test
where
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Distribution.TestSuite.QuickCheck

import Codec.JT65
import Codec.MsgJT
import Codec.PackJT
import Codec.PackISO
import Codec.PackJTExtern
import Codec.MsgJT

instance Arbitrary PlainText
  where
    arbitrary = fmap PlainText $ vectorOf 13 $ elements plainTextChar

instance Arbitrary CallSign
  where
    arbitrary = fmap CallSign $ flip suchThat (isNotE9) $ mapM elements
     [ alphaNumBlankChar
     , alphaNumChar
     , numChar
     , alphaBlankChar
     , alphaBlankChar
     , alphaBlankChar
     ]
      where
        isNotE9 str = case str of
          ('E':'9':_) -> False
          (' ':'E':'9':_) -> False      
          _ -> True
      
instance Arbitrary Message
  where
    arbitrary = frequency [
      (  1, fmap PlainTextMessage $ arbitrary)
     ,( 10, Blocks <$> arbitrary <*> arbitrary <*> arbitrary)
     ]

instance Arbitrary Block1
  where
    arbitrary = frequency
     [
      (1 , return CQDX)
     ,(10, fmap CS $ arbitrary)
     ,(1 , return $ CQ Nothing)
     ,(1 , fmap (CQ . Just) $ vectorOf 2 $ choose ('A','Z') )      
     ,(1 , return $ QRZ Nothing)
     ,(1 , fmap CQFreq $ choose (0,999))
     ,(1 , return $ DE Nothing)
     ]

instance Arbitrary Block3
  where
    arbitrary = frequency [
      ( 10, fmap Grid $ choose (0,180*180-1))
     ,( 10, fmap Report $ choose (1,30))
     ,( 10, fmap ReportR $ choose (1,30))
     ,(  1, return RO)
     ,(  1, return RRR)
     ,(  1, return R73)
     ]

isTotalIso :: Eq a => ISO a b -> a -> Bool
isTotalIso iso m
  = case roundTripFwdRev iso m of
      Right m' -> m' == m
      Left _err -> False

isTotalIsoRev :: Eq b => ISO a b -> b -> Bool
isTotalIsoRev iso m
  = case roundTripRevFwd iso m of
      Right m' -> m' == m
      Left _err -> False

prop_pack12Words :: Property
prop_pack12Words
  = forAll genPackedMessage $ isTotalIso pack12Words

prop_plainTextSplit :: PlainText -> Bool    
prop_plainTextSplit= isTotalIsoRev plainTextSplit
prop_plainText :: PlainText -> Bool
prop_plainText= isTotalIsoRev plainText
prop_callSign :: CallSign -> Bool
prop_callSign = isTotalIsoRev callSign
prop_block1 :: Block1 -> Bool
prop_block1   = isTotalIsoRev block1
prop_locator :: Block3 -> Bool
prop_locator  = isTotalIsoRev locator
prop_message :: Property
prop_message  = withMaxSuccess 10000 $ isTotalIsoRev message

genPackedMessage :: Gen PackedMessage
genPackedMessage
  = fmap (fromJust . packedMessageFromList) $ vectorOf 12 $ choose (0,63) 

prop_roundTripMessage :: Property
prop_roundTripMessage
  = forAll genPackedMessage
      $ \m -> case roundTripFwdRev message m of
        Right m' -> m'== m
        Left _err -> True

testDecode :: String -> IO PackedMessage
testDecode str = do
  (sc,_) <- callJT65Code str
  print $ fwd message sc
  return sc
  

tests :: IO [Test]
tests = return [
   testProperty "prop_plainTextSplit" prop_plainTextSplit
  ,testProperty "prop_plainText" prop_plainText
  ,testProperty "prop_callSign" prop_callSign
  ,testProperty "prop_block1" prop_block1
  ,testProperty "prop_locator" prop_locator
  ,testProperty "prop_message" prop_message
  ,testProperty "prop_roundTripMessage" prop_roundTripMessage
  ]

return []
runTests = $quickCheckAll
runTests :: IO Bool

runTestsV :: IO Bool
runTestsV = $verboseCheckAll
