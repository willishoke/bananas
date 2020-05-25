{-# LANGUAGE OverloadedStrings #-}

module Bananas where

import Parser
import Module
import Data.Word
import Data.Text (Text)
import Algebra.Graph.AdjacencyMap
import Control.Monad.State
import Codec.Audio.Wave
import Data.Attoparsec.Text
import Control.Applicative ((<|>))
import Control.Lens
import qualified Data.Sequence as Seq
import qualified Data.Map as M

-- Time flows in discrete units.

type Time = Int

-- A signal is just a list of samples (floating point values)

type Signal = [Sample]


-- This represents the state of our program.

data CompState = CompState
  { network :: AdjacencyMap Module
  , modules :: M.Map String Module 
  }

initState :: CompState
initState = CompState
  { network = empty
  , modules = M.empty
  }

main :: IO ()
main = do 
  runStateT prog initState
  return ()

-- liftIO $ print "hello" -- to do IO stuff

stack example:
  (x:xs) <- get
  put xs
  return x

-- First step is to gmap an evaluation function over the entire network.
-- You want to grab the postset of each element.
-- (This is sad, because it would be way more straightforward
-- to flip all the arrows and use the preset, but this is
-- more computationally efficient: logarithmic time to find
-- successors of a vertex vs linearithmic time to find predecessors)
-- Next we have to map over the postset.

step :: CompState -> CompState
step = undefined

type Program = StateT CompState IO ()

prog :: Program
prog = do
  liftIO $ print "Hello"
  return ()

-- Given a file name, attempt to get the contents of the file.

readBLISP :: String -> IO (AdjacencyMap Module)
readBLISP fileName = do
  contents <- readFile fileName
  pure empty -- invoke parser here

END
