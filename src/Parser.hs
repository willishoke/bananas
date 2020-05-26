{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Module

import Data.Char (isAlpha, isSpace)
import Data.Text (unpack)
import Algebra.Graph.AdjacencyMap
import Data.Attoparsec.Text
import Control.Lens
import Control.Applicative ((<|>))

import qualified Data.Map as M

{--

Constructing the ASG (abstract syntax graph) from a .nana file

Connections are "reversed" from what we'd expect.
Connecting module A to module B results in a directed edge from B to A. 
This is specifically so B can "see" A's output.

Due to the magic of algebraic graphs, we can construct the entire
graph one connection at a time, then overlay all the subgraphs.

--}


-- This represents the state of our program.

data CompState = CompState
  { _network :: AdjacencyMap Module
  , _modules :: M.Map String Module
  }

makeLenses ''CompState

initState :: CompState
initState = CompState
  { _network = empty
  , _modules = M.empty
  }

parseWithState :: CompState -> Parser CompState
parseWithState s = undefined
  

parseProgram :: Parser CompState
parseProgram = parseWithState initState
 {-- 
  = Constant
  | Adder
  | Multiplier
  | Inverter
  | Attenuator
  | Absolute
  | Integrator
  | Differentiator
  | Delay
  | Output
--}

mTypeParser :: Parser MType
mTypeParser =
      (string "Con" >> pure Constant)
  <|> (string "Add" >> pure Adder)
  <|> (string "Mul" >> pure Multiplier)
  <|> (string "Inv" >> pure Inverter)
  <|> (string "Abs" >> pure Absolute)
  <|> (string "Int" >> pure Integrator)
  <|> (string "Dif" >> pure Differentiator)
  <|> (string "Del" >> pure Delay)
  <|> (string "Out" >> pure Output)

nameParser :: Parser String 
nameParser = do
  txt <- takeWhile1 isAlpha 
  pure $ unpack txt

mapWithIntParamParser ::
  CompState -> Parser CompState
mapWithIntParamParser c = do
  t <- mTypeParser
  takeWhile1 isSpace
  i <- decimal
  takeWhile1 isSpace
  n <- nameParser
  if M.member n (c^.modules)
    then fail ("Duplicate module name " <> n)
    else let m = createWithIntParam t n i in case m of
      Just m' -> pure $ over modules (M.insert n m') c
      _       -> fail ("Module " <> n <> " takes no params")

mapParser :: CompState -> Parser CompState
mapParser c = do
  t <- mTypeParser
  takeWhile1 isSpace
  n <- nameParser
  if M.member n (c^.modules)
    then fail ("Duplicate module name " <> n)
    else let m = create t n in case m of
      Just m  -> pure $ over modules (M.insert n m) c
      Nothing -> fail ("Module " <> n <> " missing params")
 
-- Connections are reversed from the expected order.
-- This facilitates efficient checking of inputs 

connParser :: CompState -> Parser CompState
connParser c = do
  let ms = c^.modules
  m1 <- nameParser
  takeWhile1 isSpace
  m2 <- nameParser
  if not (M.member m1 ms)
    then fail (m1 <> " not declared")
    else if not (M.member m2 ms) 
           then fail (m2 <> " not declared")
           else pure $ over network (overlay 
                  (connect (vertex $ ms M.! m2) 
                           (vertex $ ms M.! m1))) c

lineParser :: CompState -> Parser CompState
lineParser c =
      (connParser c)
  <|> (mapParser c)
  <|> (mapWithIntParamParser c)

create :: MType -> String -> Maybe Module
create t s =
  case t of
    Adder           -> Just $ makeAdd s
    Multiplier      -> Just $ makeMul s
    Inverter        -> Just $ makeInv s
    Absolute        -> Just $ makeAbs s
    Integrator      -> Just $ makeIntegrator s
    Differentiator  -> Just $ makeDiff s
    Output          -> Just $ makeOut s
    _               -> Nothing

createWithIntParam :: MType -> String -> Int -> Maybe Module
createWithIntParam t s i =
  case t of
    Delay    -> Just $ makeDel s i
    _        -> Nothing

