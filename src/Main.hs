module Main where

import           Protolude

import           Data.String         (String)
import           Options.Applicative
import qualified Text.Parsec.String  as P

import           Interpreter         (InterpreterOpts (..), input, qubits, run,
                                      runInterpreter)
import           Parser              (parseInstructions)
import           State               (MachineState (..), makeQuantumState)

-- Parser for program options
interpreterOpts :: Parser InterpreterOpts
interpreterOpts =
  InterpreterOpts <$>
  strOption
    (long "input" <> short 'i' <> metavar "FILENAME" <> help "Input file") <*>
  Options.Applicative.option
    auto
    (long "qubits" <> short 'q' <> help "Number of qubits to use" <>
     metavar "INT") <*>
  switch (long "verbose" <> short 'v' <> help "Whether to be verbose")

-- Entry point for application
main :: IO ()
main = interpret =<< execParser opts
  where
    opts =
      info
        (interpreterOpts <**> helper)
        (fullDesc <> progDesc "Run quantum programs using basic QUIL operations" <>
         header "quantum-interpreter - interpreter for basic QUIL")

-- Print the final quantum state
printFinalState :: (a, MachineState) -> IO ()
printFinalState (_, s) = do
  putStrLn (show (quantumState s) :: String)

-- Handle options and run interpreter monad stack
interpret :: InterpreterOpts -> IO ()
interpret opts =
  P.parseFromFile parseInstructions inputFile >>=
  either
    reportErr
    (\ins ->
       printFinalState =<<
       (runStateT
          (runReaderT (runInterpreter run) opts)
          (initialState (qubits opts) ins)))
  where
    inputFile = input opts
    reportErr err = do
      hPutStrLn stderr $ "Error during parse: " ++ show err
      exitFailure
    initialState qs ins =
      MachineState
        { quantumState = makeQuantumState qs
        , instructions = ins
        , pc = 0
        , running = True
        }
