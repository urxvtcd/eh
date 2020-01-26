module Buffer.DeleteCommandSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Buffer.Buffer as Buff
import Buffer.DeleteCommand (deleteCommand)

import qualified Buffer.Helpers as Help

runDelete ∷ Buff.Buffer → Buff.Address → (Buff.Buffer, [String])
runDelete = Help.runCommand deleteCommand

testBuffer ∷ Buff.Buffer
testBuffer = Buff.Buffer 1 ["a", "b", "c"]

spec :: Spec
spec = do
    describe "deleteCommand" $ do
        it "should delete first line" $ do
            let address = Help.makeLineAddress testBuffer 1
                (buffer, output) = runDelete testBuffer address

            buffer `shouldBe` (Buff.Buffer 1 ["b", "c"])
            output `shouldBe` []

        it "should delete second line" $ do
            let address = Help.makeLineAddress testBuffer 2
                (buffer, output) = runDelete testBuffer address

            buffer `shouldBe` (Buff.Buffer 2 ["a", "c"])
            output `shouldBe` []

        it "should delete third line" $ do
            let address = Help.makeLineAddress testBuffer 3
                (buffer, output) = runDelete testBuffer address

            buffer `shouldBe` (Buff.Buffer 2 ["a", "b"])
            output `shouldBe` []

        it "deletes the only line" $ do
            let oneLineBuffer = (Buff.Buffer 1 ["a"])
                address = Help.makeLineAddress oneLineBuffer 1
                (buffer, output) = runDelete oneLineBuffer address

            buffer `shouldBe` (Buff.Buffer 1 [])
            output `shouldBe` []

        it "deletes a range of lines starting at first" $ do
            let address = Help.makeRangeAddress testBuffer 1 2
                (buffer, output) = runDelete testBuffer address

            buffer `shouldBe` (Buff.Buffer 1 ["c"])
            output `shouldBe` []

        it "deletes a range of lines ending at the last" $ do
            let address = Help.makeRangeAddress testBuffer 2 3
                (buffer, output) = runDelete testBuffer address

            buffer `shouldBe` (Buff.Buffer 1 ["a"])
            output `shouldBe` []

        it "deletes all the lines" $ do
            let address = Help.makeRangeAddress testBuffer 1 3
                (buffer, output) = runDelete testBuffer address

            buffer `shouldBe` (Buff.Buffer 1 [])
            output `shouldBe` []
