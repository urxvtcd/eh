module Main (main) where

import qualified System.IO as IO
import qualified System.Console.Haskeline as HL
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
            HL.runInputT HL.defaultSettings (mainLoop b)
        _ → putStrLn "invalid invocation"


mainLoop ∷ Buff.Buffer → HL.InputT IO ()
mainLoop buff = do
    commandString ← prompt
    case P.parse commands commandString of
        Nothing → do
            HL.outputStrLn "couldn't parse command"
            mainLoop buff
        Just (address, command) → do
            let (newBuff, out) = executeCommand buff address command
            _ ← mapM HL.outputStrLn out
            mainLoop newBuff


executeCommand ∷ Buff.Buffer → Buff.UnverifiedAddress → Command.Command → (Buff.Buffer, [String])
executeCommand b a c =
    case Buff.verifyAddress b a of
        Nothing → (b, ["invalid address"])
        Just vAddress → W.runWriter (Command.run c b vAddress)


prompt :: HL.InputT IO String
prompt = HL.handle (\HL.Interrupt → prompt) (HL.withInterrupt prompt')
  where
    prompt' :: HL.InputT IO String
    prompt' = do
        input ← HL.getInputLine ":"
        case input of
            Nothing → return ""
            Just x → return x
