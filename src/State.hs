module State where

import           Protolude

import           Data.Complex (Complex (..))
import qualified Data.Vector  as V

import           Instruction  (Instruction)

data MachineState =
  MachineState
    { quantumState :: QuantumState
    , instructions :: [Instruction]
    , pc           :: Integer
    , running      :: Bool
    }

type QuantumState = V.Vector (Complex Double)

-- Construct a quantum state given number of qubits
makeQuantumState :: Int -> QuantumState
makeQuantumState len =
  V.generate
    (2 ^ len)
    (\i ->
       if i == 0
         then (1 :+ 0)
         else (0 :+ 0))
