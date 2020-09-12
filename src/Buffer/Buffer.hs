{-|
 This module provides a way to act on files loaded to memory.
 -}

module Buffer.Buffer
    ( Buffer(..)
    , Address
    , AddressView(..)
    , verifyAddress
    , getLines
    , addrToBuff
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
    = Line Buffer Int
    | Range Buffer Int Int
    deriving (Eq, Show)

-- |'AddressView' is used to inspect values of 'Address'; it's necessary because
-- we don't export 'Address' data constructors, only 'verifyAddress' smart constructor.
data AddressView
    = LineView Buffer Int
    | RangeView Buffer Int Int
    deriving (Eq, Show)

instance View Address AddressView where
    view (Line buffer n) = LineView buffer n
    view (Range buffer start end) = RangeView buffer start end

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
    | n >= 1 && n <= (length (bufLines buffer)) = Just (Line buffer n)
    | otherwise = Nothing

verifyAddress buffer (UnverifiedRange start end)
    | start <= end && isStartValid && isEndValid = Just (Range buffer start end)
    | otherwise = Nothing
  where
    isStartValid = isValid start
    isEndValid = isValid end
    isValid n = isJust (verifyAddress buffer (UnverifiedLine n))


-- |'getLines' retrieves from the buffer the lines pointed by the address.
getLines ∷ Address → [String]
getLines (Line buffer n) = getLines (Range buffer n n)
getLines (Range (Buffer _ xs) start end) = drop (start - 1) (take end xs)


-- |'addrToBuff' is a helper extracting buffer from a valid address.
addrToBuff ∷ Address → Buffer
addrToBuff (Line buffer _) = buffer
addrToBuff (Range buffer _ _) = buffer
