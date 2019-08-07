module Gates where

import           Protolude

import           Control.Exception (assert)
import           Data.Complex      (Complex (..), magnitude)
import           Data.List         ((!!))
import qualified Data.Matrix       as M
import qualified Data.Vector       as V
import           System.Random     (randomIO)

import           Instruction       (Operation, Qubit, Qubits)
import           State             (QuantumState)

-- Dimension in qubits by taking the log base 2
dimensionQubits :: Integral a => a -> Int
dimensionQubits = ceiling . logBase 2 . fromIntegral

i :: Operation
i = M.identity 2

x :: Operation
x = M.fromLists [[0, 1], [1, 0]]

h :: Operation
h = M.scaleMatrix (1 / sqrt 2) $ M.fromLists [[1, 1], [1, -1]]

cnot :: Operation
cnot = M.fromLists [[1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1], [0, 0, 1, 0]]

swap :: Operation
swap = M.fromLists [[1, 0, 0, 0], [0, 0, 1, 0], [0, 1, 0, 0], [0, 0, 0, 1]]

-- Apply an operation to a quantum state by matrix multiplication
applyOperation :: Operation -> QuantumState -> QuantumState
applyOperation g s = M.getCol 1 $ g * M.colVector s

-- Compose operators by doing matrix multiplication
-- NOTE: (*) operator chosen over multStd for possible speed improvements for larger matrices
composeOperations :: Operation -> Operation -> Operation
composeOperations = (*)

-- Sample quantum state, randomly selecting a state, weighted by |phi_i|^2
sample :: MonadIO m => QuantumState -> m Int
sample s = do
  r <- liftIO $ randomIO
  return $ sampleIter r 0
  where
    sampleIter :: Double -> Int -> Int
    sampleIter r t =
      let newR = r - (magnitude (s V.! t)) ** 2
       in if newR < 0
            then t
            else sampleIter newR (t + 1)

-- Collapse the quantum state into the state at the given index
collapse :: QuantumState -> Int -> QuantumState
collapse s i =
  V.generate
    (V.length s)
    (\x ->
       if x == i
         then (1 :+ 0)
         else (0 :+ 0))

-- Observe the quantum state and collapse into that state
observe :: MonadIO m => QuantumState -> m QuantumState
observe s = do
  i <- sample s
  return $ collapse s i

-- Oh, to have dependent types
kroneckerMultiply :: (Num a) => M.Matrix a -> M.Matrix a -> M.Matrix a
kroneckerMultiply a b = M.flatten $ M.mapPos (\_ -> flip M.scaleMatrix b) a

-- TODO: re-eval constraints
kroneckerExpt :: (Num a) => M.Matrix a -> Int -> M.Matrix a
kroneckerExpt a n
  | n < 1 = M.fromLists [[1]]
  | n == 1 = a
  | otherwise = kroneckerMultiply (kroneckerExpt a (n - 1)) a

-- Lift an operation by padding with identity
liftOperation :: Operation -> Qubit -> Int -> Operation
liftOperation a index n =
  assert (index < n) kroneckerMultiply left (kroneckerMultiply a right)
  where
    left = kroneckerExpt i (n - index - (dimensionQubits $ M.nrows a))
    right = kroneckerExpt i index

-- Calculate needed transpositions from given permutation
permutationToTranspositions :: [Int] -> [(Int, Int)]
permutationToTranspositions p = permutationToTranspositionsIter 0
  where
    permutationToTranspositionsIter dest
      | dest == (length p) = []
      | otherwise =
        (case compare src dest of
           LT -> [(src, dest)]
           GT -> [(dest, src)]
           _  -> []) ++
        permutationToTranspositionsIter (dest + 1)
      where
        src = nextSrc (p !! dest) dest
    nextSrc s d
      | s < d = nextSrc (p !! s) d
      | otherwise = s

-- Convert transpositions to list of adjacent transpositions
transpositionToAdjacentTranspositions :: [(Int, Int)] -> [Int]
transpositionToAdjacentTranspositions ts = foldMap expandCons ts
  where
    expandCons (x, y)
      | y - x == 1 = [x]
      | otherwise =
        let trans = [x .. y - 1]
         in trans ++ (reverse (initSafe trans))

-- Apply 1 qubit gate
apply1QGate :: QuantumState -> Operation -> Qubit -> QuantumState
apply1QGate s a q =
  applyOperation (liftOperation a q (dimensionQubits (length s))) s

-- Apply multiple qubit gate
applynQGate :: QuantumState -> Operation -> Qubits -> QuantumState
applynQGate s a qubits = do
  applyOperation upq s
  where
    n :: Int
    n = dimensionQubits (length s)
    transpositionsToOperation :: [Int] -> Operation
    transpositionsToOperation ts =
      foldr composeOperations (M.identity (2 ^ n)) $
      map (\x -> liftOperation Gates.swap x n) ts
    u01 :: Operation
    u01 = liftOperation a 0 n
    fromSpace :: [Int]
    fromSpace =
      (reverse qubits) ++ (filter (\x -> not (elem x qubits)) [0 .. n - 1])
    trans :: [Int]
    trans =
      transpositionToAdjacentTranspositions . permutationToTranspositions $
      fromSpace
    toFrom :: Operation
    toFrom = transpositionsToOperation trans
    fromTo :: Operation
    fromTo = transpositionsToOperation (reverse trans)
    upq :: Operation
    upq = composeOperations toFrom (composeOperations u01 fromTo)

-- Apply gate, picking apply1QGate or applynQGate based on size
applyGate :: QuantumState -> Operation -> [Int] -> QuantumState
applyGate s a qubits =
  assert
    (length qubits == dimensionQubits (M.nrows a))
    (if length qubits == 1
       then apply1QGate s a (qubits !! 0)
       else applynQGate s a qubits)
