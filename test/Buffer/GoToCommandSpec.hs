module Buffer.GoToCommandSpec where

import           Test.Hspec (Spec, describe, it, shouldBe)

import qualified Buffer.Buffer as Buff
import           Buffer.Commands (goTo)

import qualified Buffer.Helpers as Help

runGoTo ∷ Buff.Address → (Buff.Buffer, [String])
runGoTo = Help.runCommand goTo

testBuffer ∷ Buff.Buffer
testBuffer = Buff.Buffer 1 ["a", "b", "c"]

spec ∷ Spec
spec = do
    describe "goto command" $ do
        it "should work on first line" $ do
            let address = Help.makeLineAddress testBuffer 1
                (buffer, output) = runGoTo address

            Buff.cursor buffer `shouldBe` 1
            output `shouldBe` []

        it "should work on last line" $ do
            let address = Help.makeLineAddress testBuffer 3
                (buffer, output) = runGoTo address

            Buff.cursor buffer `shouldBe` 3
            output `shouldBe` []

        it "should fail when given range" $ do
            let address = Help.makeRangeAddress testBuffer 1 2
                (buffer, output) = runGoTo address

            buffer `shouldBe` testBuffer
            output `shouldBe` ["?"]
