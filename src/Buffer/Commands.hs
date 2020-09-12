{-|
 This module defines commands acting on a buffer.
 -}
module Buffer.Commands
    ( Command
    , delete
    , goTo
    , print'
    , name
    , run
    ) where

import           Control.Monad.Writer (tell, Writer)

import qualified Buffer.Buffer as Buff
import           View (view)

-- |Command is a type representing command acting on a buffer.
data Command = Command
    { -- | Name of the command
      name ∷ String
      -- |'run' runs a command on an address within a buffer.
      -- It returns modified buffer along with generated output.
    , run ∷ Buff.Address → Writer [String] Buff.Buffer
    }

instance Eq Command where
    c1 == c2 = name c1 == name c2

instance Show Command where
    show c = "(Buffer Command: " ++ name c ++ ")"

-- |'delete' command implements linewise deletion.
delete ∷ Command
delete = Command "delete" delete'

delete' ∷ Buff.Address → Writer [String] Buff.Buffer
delete' address = do
    let linesAfterDelete = case view address of
            Buff.LineView (Buff.Buffer _ xs) n → deleteLines xs n n
            Buff.RangeView (Buff.Buffer _ xs) start end → deleteLines xs start end
        newCursor = getCursorAfterDelete linesAfterDelete address

    return (Buff.Buffer newCursor linesAfterDelete)


getCursorAfterDelete ∷ [String] → Buff.Address → Int
getCursorAfterDelete xs address
    | length xs == 0 = 1
    | otherwise = min (getFirstLineOfAddress address) (length xs)


getFirstLineOfAddress ∷ Buff.Address → Int
getFirstLineOfAddress address = case view address of
    Buff.LineView _ n → n
    Buff.RangeView _ start _ → start


deleteLines ∷ [String] → Int → Int → [String]
deleteLines xs start end = prefix ++ suffix
  where
    (prefix, rest) = splitAt (start - 1) xs
    suffix = drop (end - start + 1) rest


-- |'goTo' command sets cursor on a given line in a buffer.
goTo ∷ Command
goTo = Command "goto" goTo'

goTo' ∷ Buff.Address → Writer [String] Buff.Buffer
goTo' address = case view address of
    Buff.LineView (Buff.Buffer _ bufLines) n → do
        return (Buff.Buffer n bufLines)
    Buff.RangeView buffer _ _ → do
        tell ["?"]
        return buffer


-- |'print'' command prints lines from a buffer.
-- It's named 'print'' to avoid annoying conflicts with 'Prelude.print'.
print' ∷ Command
print' = Command "print" print''

print'' ∷ Buff.Address → Writer [String] Buff.Buffer
print'' address = do
    tell $ Buff.getLines address
    return $ Buff.addrToBuff address
