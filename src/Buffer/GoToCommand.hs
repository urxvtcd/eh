module Buffer.GoToCommand (goToCommand) where

import Control.Monad.Writer (Writer, tell)

import qualified Buffer.Buffer as Buff
import View (view)

goToCommand ∷ Buff.Command
goToCommand = Buff.Command "" goTo


goTo ∷ Buff.Buffer → Buff.Address → Writer [String] Buff.Buffer

goTo buffer@(Buff.Buffer _ bufLines) address = case view address of
    Buff.LineView n → do
        return (Buff.Buffer n bufLines)
    Buff.RangeView _ _ → do
        tell ["?"]
        return buffer
