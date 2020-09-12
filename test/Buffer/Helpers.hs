module Buffer.Helpers
    ( runCommand
    , makeLineAddress
    , makeRangeAddress
    ) where

import           Control.Monad.Writer (runWriter)
import           Data.Maybe (fromJust)

import qualified Buffer.Buffer as B
import qualified Buffer.Commands as C


runCommand ∷ C.Command → B.Address → (B.Buffer, [String])
runCommand command address = runWriter $ C.run command address

makeLineAddress ∷ B.Buffer → Int → B.Address
makeLineAddress buffer n =
    fromJust $ B.verifyAddress buffer (B.UnverifiedLine n)

makeRangeAddress ∷ B.Buffer → Int → Int → B.Address
makeRangeAddress buffer start end =
    fromJust $ B.verifyAddress buffer (B.UnverifiedRange start end)
