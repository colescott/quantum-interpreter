module Interpreter where

import           Protolude

import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State  (StateT)
import           Data.List                  ((!!))
import           Data.String

import           Gates                      (applyGate, observe)
import           Instruction                (Instruction (..))
import           State                      (MachineState, instructions, pc,
                                             quantumState, running)

-- Options for interpreter
data InterpreterOpts =
  InterpreterOpts
    { input   :: String
    , qubits  :: Int
    , verbose :: Bool
    }

-- Interpreter monad stack
newtype Interpreter a =
  Interpreter
    { runInterpreter :: ReaderT InterpreterOpts (StateT MachineState IO) a
    }
  deriving ( Monad
           , Applicative
           , Functor
           , MonadIO
           , MonadState MachineState
           , MonadReader InterpreterOpts
           )

-- Run interpreter and exit when PC falls off or HALT received
run :: Interpreter ()
run = do
  ins <- (\s -> ((!!) (instructions s)) . fromIntegral . pc $ s) <$> get
  --when (verbose opts) (hPutStrLn stderr ins)
  case ins of
    Gate a qs ->
      modify
        (\s -> s {quantumState = applyGate (quantumState s) a qs, pc = pc s + 1})
    Measure -> do
      out <- observe =<< quantumState <$> get
      modify (\s -> s {quantumState = out, pc = pc s + 1})
    Halt -> modify (\s -> s {running = False})
  -- If pc fell off then set running to false
  fellOff <-
    (\s -> ((fromIntegral . pc $ s) == (length . instructions $ s))) <$> get
  when fellOff (modify (\s -> s {running = False}))
  -- Recurse if interpreter is still running
  shouldRun <- running <$> get
  when shouldRun run
