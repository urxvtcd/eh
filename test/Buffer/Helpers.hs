module Buffer.Helpers
    ( runCommand
    , makeLineAddress
    , makeRangeAddress
    ) where

import Control.Monad.Writer (runWriter)
import Data.Maybe (fromJust)

import qualified Buffer.Buffer as B


runCommand ∷ B.Command → B.Buffer → B.Address → (B.Buffer, [String])
runCommand command buffer address = runWriter $ B.run command buffer address

makeLineAddress ∷ B.Buffer → Int → B.Address
makeLineAddress buffer n =
    fromJust $ B.verifyAddress buffer (B.UnverifiedLine n)

makeRangeAddress ∷ B.Buffer → Int → Int → B.Address
makeRangeAddress buffer start end =
    fromJust $ B.verifyAddress buffer (B.UnverifiedRange start end)
