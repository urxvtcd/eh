module Buffer.DeleteCommand (deleteCommand) where

import Control.Monad.Writer (Writer)

import qualified Buffer.Buffer as Buff
import View (view)

deleteCommand ∷ Buff.Command
deleteCommand = Buff.Command "d" delete


delete ∷ Buff.Buffer → Buff.Address → Writer [String] Buff.Buffer
delete (Buff.Buffer _ xs) address = do
    let linesAfterDelete = deleteLines xs address
        newCursor = getCursorAfterDelete linesAfterDelete address

    return (Buff.Buffer newCursor linesAfterDelete)


getCursorAfterDelete ∷ [String] → Buff.Address → Int
getCursorAfterDelete xs address = if len == 0
    then 1
    else min (getFirstLineOfAddress address) len
  where
    len = length xs


getFirstLineOfAddress ∷ Buff.Address → Int
getFirstLineOfAddress address = case view address of
    Buff.LineView n → n
    Buff.RangeView start _ → start


deleteLines ∷ [String] → Buff.Address → [String]
deleteLines xs address = case Buff.view address of
    Buff.LineView n → deleteLines' xs n n
    Buff.RangeView start end → deleteLines' xs start end


deleteLines' ∷ [String] → Int → Int → [String]
deleteLines' xs start end = prefix ++ suffix
  where
    (prefix, rest) = splitAt (start - 1) xs
    suffix = drop (end - start + 1) rest
