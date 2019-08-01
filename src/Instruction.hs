module Instruction where

import           Protolude

import           Data.Complex (Complex (..))
import qualified Data.Matrix  as M

-- NOTE: could be newtyped for more safety
type Qubit = Int

type Qubits = [Qubit]

type Operation = M.Matrix (Complex Double)

data Instruction
  = Gate Operation Qubits
  | Measure
  | Halt
  deriving (Show)
