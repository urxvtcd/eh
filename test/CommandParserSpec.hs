module CommandParserSpec where

import           Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)

import           CommandParser (parse)

import qualified Buffer.Buffer as Buff
import qualified Buffer.Commands as C


commands ∷ Map.Map String C.Command
commands = Map.fromList
    [ ("d", C.delete)
    , ("", C.goTo)
    , ("p", C.print')
    ]


spec ∷ Spec
spec = do
    describe "parse" $ do
        it "should parse line address and command" $ do
            let (address, command) = fromJust (parse commands "2p")
            address `shouldBe` Buff.UnverifiedLine 2
            C.name command `shouldBe` "print"

        it "should parse line address and command with whitespace" $ do
            let (address, command) = fromJust (parse commands "  3     p  ")
            address `shouldBe` Buff.UnverifiedLine 3
            C.name command `shouldBe` "print"

        it "should parse range address and command" $ do
            let (address, command) = fromJust (parse commands "5,10p")
            address `shouldBe` Buff.UnverifiedRange 5 10
            C.name command `shouldBe` "print"

        it "should parse range address and command with whitespace" $ do
            let (address, command) = fromJust (parse commands " 2 ,   3 p     ")
            address `shouldBe` Buff.UnverifiedRange 2 3
            C.name command `shouldBe` "print"

        it "should parse address and command with empty identifier" $ do
            let (address, command) = fromJust (parse commands "4")
            address `shouldBe` Buff.UnverifiedLine 4
            C.name command `shouldBe` "goto"

        it "should fail if address is empty" $ do
            let parsed = parse commands "p"
            parsed `shouldBe` Nothing

        it "should fail on incomplete address" $ do
            let parsed = parse commands "1,p"
            parsed `shouldBe` Nothing

            let parsed' = parse commands ",2p"
            parsed' `shouldBe` Nothing

            let parsed'' = parse commands ",p"
            parsed'' `shouldBe` Nothing
