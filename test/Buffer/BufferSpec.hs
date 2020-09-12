module Buffer.BufferSpec where

import           Test.Hspec (Spec, describe, it, shouldBe)

import qualified Buffer.Buffer as Buff

testBuffer ∷ Buff.Buffer
testBuffer = Buff.Buffer 0 ["a", "b", "c"]

isLineValid ∷ Buff.Buffer → Int → Bool
isLineValid buffer n = case address of
    Just _ → True
    _ → False
  where
    address = Buff.verifyAddress buffer (Buff.UnverifiedLine n)

isRangeValid ∷ Buff.Buffer → Int → Int → Bool
isRangeValid buffer start end = case address of
    Just _ → True
    _ → False
  where
    address = Buff.verifyAddress buffer (Buff.UnverifiedRange start end)

spec ∷ Spec
spec = do
    describe "verifyAddress" $ do
        it "should return Nothing for zeroth line" $ do
            isLineValid testBuffer 0 `shouldBe` False

        it "should work on first line" $ do
            isLineValid testBuffer 1 `shouldBe` True

        it "should work on the last line" $ do
            isLineValid testBuffer 3 `shouldBe` True

        it "should fail on line after the last one" $ do
            isLineValid testBuffer 4 `shouldBe` False

        it "should fail if first range component is invalid" $ do
            isRangeValid testBuffer 0 1 `shouldBe` False

        it "should fail if second range component is invalid" $ do
            isRangeValid testBuffer 0 4 `shouldBe` False

        it "should work when range exists" $ do
            isRangeValid testBuffer 1 2 `shouldBe` True

        it "should fail if range is backwards" $ do
            isRangeValid testBuffer 2 1 `shouldBe` False
