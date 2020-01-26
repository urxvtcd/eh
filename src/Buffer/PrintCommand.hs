module Buffer.PrintCommand (printCommand) where

import Control.Monad.Writer (Writer, tell)

import qualified Buffer.Buffer as Buff


printCommand ∷ Buff.Command
printCommand = Buff.Command "p" print'

print' ∷ Buff.Buffer → Buff.Address → Writer [String] Buff.Buffer
print' buffer address = do
    tell $ Buff.getLines buffer address
    return buffer
