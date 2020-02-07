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

data Command = Command
    { name ∷ String
    , run ∷ Buff.Buffer → Buff.Address → Writer [String] Buff.Buffer
    }

instance Eq Command where
    c1 == c2 = name c1 == name c2

instance Show Command where
    show c = "(Buffer Command: " ++ name c ++ ")"

delete ∷ Command
delete = Command "delete" delete'

delete' ∷ Buff.Buffer → Buff.Address → Writer [String] Buff.Buffer
delete' (Buff.Buffer _ xs) address = do
    let linesAfterDelete = case Buff.view address of
            Buff.LineView n → deleteLines xs n n
            Buff.RangeView start end → deleteLines xs start end
        newCursor = getCursorAfterDelete linesAfterDelete address

    return (Buff.Buffer newCursor linesAfterDelete)


getCursorAfterDelete ∷ [String] → Buff.Address → Int
getCursorAfterDelete xs address
    | length xs == 0 = 1
    | otherwise = min (getFirstLineOfAddress address) (length xs)


getFirstLineOfAddress ∷ Buff.Address → Int
getFirstLineOfAddress address = case view address of
    Buff.LineView n → n
    Buff.RangeView start _ → start


deleteLines ∷ [String] → Int → Int → [String]
deleteLines xs start end = prefix ++ suffix
  where
    (prefix, rest) = splitAt (start - 1) xs
    suffix = drop (end - start + 1) rest


goTo ∷ Command
goTo = Command "goto" goTo'

goTo' ∷ Buff.Buffer → Buff.Address → Writer [String] Buff.Buffer
goTo' buffer@(Buff.Buffer _ bufLines) address = case view address of
    Buff.LineView n → do
        return (Buff.Buffer n bufLines)
    Buff.RangeView _ _ → do
        tell ["?"]
        return buffer


-- named print' to avoid annoying conflicts with Prelude.print
print' ∷ Command
print' = Command "print" print''

print'' ∷ Buff.Buffer → Buff.Address → Writer [String] Buff.Buffer
print'' buffer address = do
    tell $ Buff.getLines buffer address
    return buffer
