{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Module

import Prelude hiding (takeWhile)
import Data.Char (isAlpha, isDigit, isSpace)

import Data.Attoparsec.Text

import Algebra.Graph.AdjacencyMap

import Control.Lens
import Control.Monad
import Control.Applicative ((<|>))

import qualified Data.Text as T
import qualified Data.Map as M


-- parser
-- constructing the ASG (abstract syntax graph) from a .nana file


-- state holds the current ASG and bindings of variable names to modules.
-- connections are "reversed" from what we'd expect.
-- connecting module A to module B results in a directed edge from B to A. 
-- this is specifically so B can "see" A's output.
-- due to the magic of algebraic graphs, we can construct the entire
-- graph one connection at a time, then overlay all the subgraphs.

data CompState = CompState
  { _network :: AdjacencyMap Module
  , _modules :: M.Map String Module
  }
  deriving (Eq, Show)

makeLenses ''CompState

initState :: CompState
initState = CompState
  { _network = empty
  , _modules = M.empty
  }


-- parse an entire nana program
-- first case is end of file
-- second case is empty line
-- third case, something to parse!

parseLines :: [T.Text] -> CompState -> CompState
parseLines xs c
  | null xs = c             
  | T.null (head xs) = parseLines (tail xs) c
  | otherwise =
      let res = parseOnly (lineParser c) (head xs)
      in case res of
        Right r -> parseLines (tail xs) r
        Left l  -> error l
    
 
-- parse a single line
-- four possibilities: 
--   - variable declaration
--   - variable declaration with parameter
--   - regular connection
--   - connection with constant

lineParser :: CompState -> Parser CompState
lineParser c =
      (connParser c)
  <|> (connConstParser c)
  <|> (declParser c)
  <|> (declNumParser c)



-- this parses a variable declaration with an integer parameter

declNumParser :: CompState -> Parser CompState
declNumParser c = do
  n <- nameParser
  takeWhile1 isSpace
  char ':'
  takeWhile1 isSpace
  t <- mTypeParser
  takeWhile1 isSpace
  i <- decimal
  when (M.member n $ c^.modules) $ 
    fail $ "Duplicate module name " <> n
  let m = createNumParam t n i in case m of
    Just m' -> pure $ over modules (M.insert n m') c
    _       -> fail $ "Module " <> n <> " takes no params"


-- this parses a variable declaration

declParser :: CompState -> Parser CompState
declParser c = do
  n <- nameParser
  takeWhile1 isSpace
  char ':'
  takeWhile1 isSpace
  t <- mTypeParser
  endOfInput
  when (M.member n $ c^.modules) $ 
    fail $ "Duplicate module name " <> n
  let m = create t n in case m of
    Just m  -> pure $ over modules (M.insert n m) c
    Nothing -> fail ("Module " <> n <> " missing params")


-- parse a connection between two modules 
-- remember connections are reversed!

connParser :: CompState -> Parser CompState
connParser c = do
  let ms = c^.modules
  m1 <- nameParser
  takeWhile1 isSpace
  m2 <- nameParser
  when (not $ M.member m1 ms) $
    fail (m1 <> " not declared")
  when (not $ M.member m2 ms) $ 
    fail (m2 <> " not declared")
  pure $ flip (over network) c $ overlay $ connect
    (vertex $ ms M.! m2) 
    (vertex $ ms M.! m1)


-- parse a connection between a constant and any other module

connConstParser :: CompState -> Parser CompState
connConstParser c = do
  let ms = c^.modules
  n <- double 
  takeWhile1 isSpace
  m1 <- nameParser
  when (not $ M.member m1 ms) $
    fail $ m1 <> " not declared"
  pure $ flip (over network) c $ overlay $ connect
    (vertex $ ms M.! m1)
    (vertex $ makeConst (show n) n) 


-- type declarations must be uppercase
-- each type has unique three-letter id

mTypeParser :: Parser MType
mTypeParser =
      (string "ADD" >> pure Adder)
  <|> (string "MUL" >> pure Multiplier)
  <|> (string "INV" >> pure Inverter)
  <|> (string "ABS" >> pure Absolute)
  <|> (string "INT" >> pure Integrator)
  <|> (string "DIF" >> pure Differentiator)
  <|> (string "DEL" >> pure Delay)
  <|> (string "CLK" >> pure Clock)
  <|> (string "SIN" >> pure Sine)
  <|> (string "OUT" >> pure Output)


-- parse a variable name

nameParser :: Parser String 
nameParser = do
  txt <- takeWhile (\x -> isAlpha x || isDigit x)
  pure $ T.unpack txt


-- modules that take no parameters

create :: MType -> String -> Maybe Module
create t s = case t of
  Adder           -> Just $ makeAdd s
  Multiplier      -> Just $ makeMul s
  Inverter        -> Just $ makeInv s
  Absolute        -> Just $ makeAbs s
  Integrator      -> Just $ makeInt s
  Differentiator  -> Just $ makeDif s
  Output          -> Just $ makeOut s
  _               -> Nothing


-- each of these modules take an integer parameter when declared

createNumParam :: MType -> String -> Int -> Maybe Module
createNumParam t s i = case t of
  Delay -> Just $ makeDel s i
  Clock -> Just $ makeClk s i
  Sine  -> Just $ makeSin s i
  _     -> Nothing

