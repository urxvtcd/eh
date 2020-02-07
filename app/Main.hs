module Main (main) where

import qualified System.IO as IO
import qualified System.Environment as Env
import qualified Control.Monad.Writer as W
import qualified Data.Map as Map

import qualified Buffer.Buffer as Buff
import qualified Buffer.Commands as Command

import qualified CommandParser as P

commands ∷ Map.Map String Command.Command
commands = Map.fromList
    [ ("d", Command.delete)
    , ("", Command.goTo)
    , ("p", Command.print')
    ]

main :: IO ()
main = do
    arguments ← Env.getArgs
    case arguments of
        [filename] → do
            contents ← IO.readFile filename
            let b = Buff.Buffer 0 (lines contents)
            mainLoop b
        _ → putStrLn "invalid invocation"


mainLoop ∷ Buff.Buffer → IO ()
mainLoop buff = do
    commandString ← prompt
    case P.parse commands commandString of
        Nothing → do
            putStrLn "couldn't parse command"
            mainLoop buff
        Just (address, command) → do
            let (newBuff, out) = executeCommand buff address command
            _ ← mapM putStrLn out
            mainLoop newBuff


executeCommand ∷ Buff.Buffer → Buff.UnverifiedAddress → Command.Command → (Buff.Buffer, [String])
executeCommand b a c =
    case Buff.verifyAddress b a of
        Nothing → (b, ["invalid address"])
        Just vAddress → W.runWriter (Command.run c b vAddress)

prompt :: IO String
prompt = do
    putStr ":"
    IO.hFlush IO.stdout
    getLine
