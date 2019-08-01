module Parser where

import           Protolude

import           Control.Applicative ((<|>))
import           Control.Monad       (void)
import           Data.Char           (digitToInt)
import qualified Text.Parsec         as P
import qualified Text.Parsec.String  as P

import qualified Gates
import           Instruction         (Instruction (..), Qubit)

-- Parser for instructions separated by newlines
parseInstructions :: P.Parser [Instruction]
parseInstructions =
  P.many1 ((whitespace *> parseInstruction) <* P.many (void P.newline)) <* P.eof

-- Parser for single instruction
parseInstruction :: P.Parser Instruction
parseInstruction = P.try (parseGate <|> parseMeasure <|> parseHalt)

-- Parser for measure instruction
parseMeasure :: P.Parser Instruction
parseMeasure = do
  _ <- P.string "MEASURE"
  return Measure

-- Parser for halt instruction
parseHalt :: P.Parser Instruction
parseHalt = do
  _ <- P.string "HALT"
  return Halt

-- Parser for all gates
parseGate :: P.Parser Instruction
parseGate = P.try (parseI <|> parseX <|> parseH <|> parseCNOT <|> parseSwap)

-- Parser for I gate
parseI :: P.Parser Instruction
parseI = do
  _ <- P.char 'I'
  whitespace
  a <- parseQubit
  return $ Gate Gates.i [a]

-- Parser for X gate
parseX :: P.Parser Instruction
parseX = do
  _ <- P.char 'X'
  whitespace
  a <- parseQubit
  return $ Gate Gates.x [a]

-- Parser for H gate
parseH :: P.Parser Instruction
parseH = do
  _ <- P.char 'H'
  whitespace
  a <- parseQubit
  return $ Gate Gates.h [a]

-- Parser for CNOT gate
parseCNOT :: P.Parser Instruction
parseCNOT = do
  _ <- P.string "CNOT"
  whitespace
  a <- parseQubit
  whitespace
  b <- parseQubit
  return $ Gate Gates.cnot [a, b]

-- Parser for SWAP gate
parseSwap :: P.Parser Instruction
parseSwap = do
  _ <- P.string "SWAP"
  whitespace
  a <- parseQubit
  whitespace
  b <- parseQubit
  return $ Gate Gates.swap [a, b]

-- Parser for single qubit index
parseQubit :: P.Parser Qubit
parseQubit = do
  parseDigits 10 P.digit
  where
    parseDigits base digitP = do
      digits <- P.many1 digitP
      let n =
            fromIntegral $
            foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
      seq n (return n)

-- | Parser for non-newline whitespace
whitespace :: P.Parser ()
whitespace = void $ P.many $ P.oneOf " \t"
