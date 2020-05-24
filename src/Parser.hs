{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Module
import Data.Word
import Data.Text (Text)
import Algebra.Graph
import Algebra.Graph.AdjacencyMap
import Data.Attoparsec.Text
import Control.Applicative ((<|>))

{--

Constructing the ASG (abstract syntax graph) from a blisp file

Connections are "reversed" from what we'd expect.
Connecting module A to module B results in a directed edge from B to A. 
This is specifically so B can "see" A's output.

Due to the magic of algebraic graphs, we can construct the entire
graph one connection at a time, then overlay all the subgraphs.

--}

patchParser :: Parser (Graph Module)
patchParser = undefined
{--
m1Parser :: Parser Module
m1Parser =
      (string "Int" >> pure integrator)
  <|> (string "Dif" >> pure differentiator)
  <|> (string "Inv" >> pure inverter)
  <|> (string "Del" >> pure delay)
--}
