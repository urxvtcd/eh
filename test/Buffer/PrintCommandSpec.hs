module Buffer.PrintCommandSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Buffer.Buffer as Buff
import Buffer.PrintCommand (printCommand)

import qualified Buffer.Helpers as Help

runPrint ∷ Buff.Buffer → Buff.Address → (Buff.Buffer, [String])
runPrint = Help.runCommand printCommand

testBuffer ∷ Buff.Buffer
testBuffer = Buff.Buffer 0 ["a", "b", "c"]

spec :: Spec
spec = do
    describe "printCommand" $ do
        it "should work on first line" $ do
            let address = Help.makeLineAddress testBuffer 1
                (buffer, output) = runPrint testBuffer address

            buffer `shouldBe` testBuffer
            output `shouldBe` ["a"]

        it "should work on the last line" $ do
            let address = Help.makeLineAddress testBuffer 3
                (buffer, output) = runPrint testBuffer address

            buffer `shouldBe` testBuffer
            output `shouldBe` ["c"]

        it "should work on range" $ do
            let address = Help.makeRangeAddress testBuffer 1 2
                (buffer, output) = runPrint testBuffer address

            buffer `shouldBe` testBuffer
            output `shouldBe` ["a", "b"]
