{-|
 This module provides a way to act on files loaded to memory.
 -}

module Buffer.Buffer
    ( Buffer(..)
    , Address
    , AddressView(..)
    , verifyAddress
    , getLines
    , UnverifiedAddress(..)
    ) where

import           Data.Maybe (isJust)

import           View (View, view)


-- |'Buffer' is a representation of file in memory opened in eh.
data Buffer = Buffer
    { cursor ∷ Int
    , bufLines ∷ [String]
    } deriving (Eq, Show)

-- |'Address' is one or multiple lines on which commands act.
-- To ensure commands operate on valid addresses you cannot create it directly,
-- use 'verifyAddress' smart constructor instead.
data Address
    = Line Int
    | Range Int Int
    deriving (Eq, Show)

-- |'AddressView' is used to inspect values of 'Address'; it's necessary because
-- we don't export 'Address' data constructors, only 'verifyAddress' smart constructor.
data AddressView
    = LineView Int
    | RangeView Int Int
    deriving (Eq, Show)

instance View Address AddressView where
    view (Line n) = LineView n
    view (Range start end) = RangeView start end

-- | 'UnverifiedAddress' represents one or multiple lines on which commands act;
-- it's created by the user, and needs to be checked with 'verifyAddress' before use.
data UnverifiedAddress
    = UnverifiedLine Int
    | UnverifiedRange Int Int
    deriving (Eq, Show)


-- | 'verifyAddress' takes a buffer and an unsafe address, and returns address
-- to be used by command if the address is valid in context of the buffer.
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


-- |'getLines' retrieves from the buffer the lines pointed by the address.
getLines ∷ Buffer → Address → [String]
getLines buffer (Line n) = getLines buffer (Range n n)
getLines (Buffer _ xs) (Range start end) = drop (start - 1) (take end xs)
