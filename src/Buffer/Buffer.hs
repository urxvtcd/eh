module Buffer.Buffer
    ( Buffer(..)
    , Address
    , view
    , AddressView(..)
    , verifyAddress
    , getLines
    , UnverifiedAddress(..)
    ) where

import           Data.Maybe (isJust)

import           View (View, view)


data Buffer = Buffer
    { cursor ∷ Int
    , bufLines ∷ [String]
    } deriving (Eq, Show)

data Address
    = Line Int
    | Range Int Int
    deriving (Eq, Show)

data AddressView
    = LineView Int
    | RangeView Int Int
    deriving (Eq, Show)

instance View Address AddressView where
    view (Line n) = LineView n
    view (Range start end) = RangeView start end

data UnverifiedAddress
    = UnverifiedLine Int
    | UnverifiedRange Int Int
    deriving (Eq, Show)


verifyAddress ∷ Buffer → UnverifiedAddress → Maybe Address

verifyAddress buffer (UnverifiedLine n)
    | n >= 1 && n <= (length (bufLines buffer)) = Just (Line n)
    | otherwise = Nothing

verifyAddress buffer (UnverifiedRange start end)
    | start <= end && isStartValid && isEndValid = Just (Range start end)
    | otherwise = Nothing
  where
    isStartValid = isValid start
    isEndValid = isValid end
    isValid n = isJust (verifyAddress buffer (UnverifiedLine n))


getLines ∷ Buffer → Address → [String]
getLines buffer (Line n) = getLines buffer (Range n n)
getLines (Buffer _ xs) (Range start end) = drop (start - 1) (take end xs)
