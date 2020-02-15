{-|
 This module implements logic necessary to parse string given by the user
 to a command to be executed on a buffer.
 -}
module CommandParser
    ( parse
    ) where

import qualified Text.ParserCombinators.ReadP as RP
import           Control.Applicative ((<|>))
import qualified Data.Char as Char
import qualified Data.Map as Map

import qualified Buffer.Buffer as Buff
import           Buffer.Commands (Command)


-- |'parse' takes a map of supported commands, an input string, and from it
-- it tries to extract a buffer address and a command to be executed on the address.
parse ∷ Map.Map String Command → String → Maybe (Buff.UnverifiedAddress, Command)
parse m i = case RP.readP_to_S (parseInput m) i of
    [(parsed, "")] → Just parsed
    _ → Nothing


parseInput ∷ Map.Map String Command → RP.ReadP (Buff.UnverifiedAddress, Command)
parseInput m = do
    _ ← RP.skipSpaces
    address ← parseAddress
    _ ← RP.skipSpaces
    command ← parseCommand m
    _ ← RP.skipSpaces
    _ ← RP.eof
    return (address, command)

parseCommand ∷ Map.Map String Command → RP.ReadP Command
parseCommand m = do
    commandStr ← RP.many (RP.satisfy Char.isLetter)
    case Map.lookup commandStr m of
        Nothing → RP.pfail
        Just command → return command

parseAddress ∷ RP.ReadP Buff.UnverifiedAddress
parseAddress = singleAddress <|> rangeAddress

singleAddress ∷ RP.ReadP Buff.UnverifiedAddress
singleAddress = do
    address ← RP.many1 (RP.satisfy Char.isDigit)
    return (Buff.UnverifiedLine (read address))

rangeAddress ∷ RP.ReadP Buff.UnverifiedAddress
rangeAddress = do
    (Buff.UnverifiedLine first) ← singleAddress
    _ ← RP.skipSpaces
    _ ← RP.string ","
    _ ← RP.skipSpaces
    (Buff.UnverifiedLine second) ← singleAddress
    return (Buff.UnverifiedRange first second)
