{-# LANGUAGE OverloadedStrings #-}

module Bananas where

import Parser
import Module

import Data.Char (isDigit)
import Data.Text (Text)
import Algebra.Graph.AdjacencyMap
import System.Environment
import Control.Monad.State
import Data.Attoparsec.Text
import Control.Lens hiding (transform)
import Data.Set (toList)

import qualified Data.Sequence as S


type Program = StateT CompState IO ()


-- MAIN

main :: IO ()
main = do
  args <- getArgs
  if length args < 2 || not (all isDigit $ args !! 1)
    then error "Usage" 
    else do
      runStateT runComp initState
      return ()

{--
stack example:
  (x:xs) <- get
  put xs
  return x
--}


-- STEP
-- Perform a single computation over the ASG
-- First step is to grab the postset of each element.
-- Convert to list, then grab all the buffers.
-- Find their first elements. These get passed along to be evaluated.
-- Thanks to pointfree.io for helping remove all the extra lambdas.

step :: CompState -> CompState
step = over network transform

transform :: AdjacencyMap Module -> AdjacencyMap Module
transform ms =
  flip gmap ms $
    let f = toList . flip postSet ms
        g = map $ flip S.index 0 . (^.buffer)
    in g . f >>= eval 


-- EVAL
-- Change state of a module on input

eval :: [Sample] -> Module -> Module
eval ss m
  | null ss = m -- No incoming connections
  | otherwise = case m^.mType of
      Constant        -> m
      Adder           -> evalAdd ss m 
      Multiplier      -> evalMul ss m 
      Inverter        -> evalInv (head ss) m
      Absolute        -> evalAbs (head ss) m
      Integrator      -> evalIntegrator (head ss) m
      Differentiator  -> evalDiff (head ss) m
      Delay           -> evalDel (head ss ) m
      Output          -> evalOut (head ss) m


runComp :: Program
runComp = do
  liftIO $ mapM_ putStrLn bananas
  pState <- get
  let g = pState^.network
  return ()

-- Given a file name, attempt to get the contents of the file.

readBLISP :: String -> IO Text
readBLISP fileName = do
  contents <- readFile fileName
  pure "" -- invoke parser here


b1 = "🍌🍌🍌      🍌     🍌    🍌     🍌     🍌    🍌     🍌     🍌🍌🍌 "
b2 = "🍌   🍌    🍌🍌    🍌🍌  🍌    🍌🍌    🍌🍌  🍌    🍌🍌   🍌      "
b3 = "🍌🍌🍌    🍌🍌🍌   🍌 🍌 🍌   🍌🍌🍌   🍌 🍌 🍌   🍌🍌🍌   🍌🍌🍌 "
b4 = "🍌   🍌  🍌    🍌  🍌  🍌🍌  🍌    🍌  🍌  🍌🍌  🍌    🍌       🍌"
b5 = "🍌🍌🍌   🍌    🍌  🍌    🍌  🍌    🍌  🍌    🍌  🍌    🍌  🍌🍌🍌 "

bananas = [b1, b2, b3, b4, b5]

-- END
