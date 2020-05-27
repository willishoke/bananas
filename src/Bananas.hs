{-# LANGUAGE OverloadedStrings #-}

import Parser
import Module

import Prelude hiding (readFile, lines)
import Data.Char (isDigit)
import Data.Text (Text, lines)
import Data.Text.IO (readFile)
import Algebra.Graph.AdjacencyMap
import System.Environment
import Control.Monad.State
import Control.Lens hiding (transform)
import Data.Foldable (toList)
import Data.WAVE

import qualified Data.Sequence as S


type Program = StateT CompState IO ()


-- MAIN

main :: IO ()
main = do
  runStateT runComp initState
  return ()


parseArgs :: IO (String, Int)
parseArgs = do
  args <- getArgs
  if length args < 2 || not (all isDigit $ args !! 1)
    then error "Usage" 
    else pure (args !! 0, read $ args !! 1)


runComp :: Program
runComp = do
  liftIO $ mapM_ putStrLn bananas
  (fileName, iterations) <- liftIO parseArgs
  pText <- liftIO $ readFile fileName
  pState <- get
  let seed = parseLines (lines pText) pState 
      computed = comp iterations seed
      sound = getOutput computed
  --liftIO $ print seed
  --liftIO $ print computed
  --liftIO $ print $ length $ head $ sound
  --liftIO $ print $ take 100 $ head $ sound
  liftIO $ toWave sound
  return ()

comp :: Int -> CompState -> CompState 
comp i c
  | i <= 0 = c
  | otherwise = comp (pred i) (step c)

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
  flip gmap ms $ g . f >>= eval where
    f = toList . flip postSet ms
    g = map $ flip S.index 0 . (^.buffer)

     
getOutput :: CompState -> [[Sample]]
getOutput c =
  let ms = vertexList (c^.network)
      outs = filter (\v -> v^.mType == Output) ms
  in map (toList . (^.buffer)) outs


toWave :: [[Sample]] -> IO ()
toWave = \ss ->
  let raw = map (map $ doubleToSample . (/2.0) . (+1)) ss
      h = WAVEHeader
            { waveNumChannels = 1
            , waveFrameRate = 20000
            , waveBitsPerSample = 8
            , waveFrames = Just $ length $ head raw
            }
      w = WAVE { waveHeader = h, waveSamples = raw }
  in putWAVEFile "test.wav" w


-- EVAL
-- Change state of a module on input

eval :: [Sample] -> Module -> Module
eval ss m = case m^.mType of
  Constant        -> m
  Adder           -> if null ss then m else evalAdd ss m 
  Multiplier      -> if null ss then m else evalMul ss m 
  Inverter        -> if null ss then m else evalInv (head ss) m
  Absolute        -> if null ss then m else evalAbs (head ss) m
  Integrator      -> if null ss then m else evalInt (head ss) m
  Differentiator  -> if null ss then m else evalDif (head ss) m
  Delay           -> if null ss then m else evalDel (head ss) m
  Clock           -> evalClk m
  Output          -> if null ss then m else evalOut (head ss) m



b1 = "🍌🍌🍌      🍌     🍌    🍌     🍌     🍌    🍌     🍌     🍌🍌🍌 "
b2 = "🍌   🍌    🍌🍌    🍌🍌  🍌    🍌🍌    🍌🍌  🍌    🍌🍌   🍌      "
b3 = "🍌🍌🍌    🍌🍌🍌   🍌 🍌 🍌   🍌🍌🍌   🍌 🍌 🍌   🍌🍌🍌   🍌🍌🍌 "
b4 = "🍌   🍌  🍌    🍌  🍌  🍌🍌  🍌    🍌  🍌  🍌🍌  🍌    🍌       🍌"
b5 = "🍌🍌🍌   🍌    🍌  🍌    🍌  🍌    🍌  🍌    🍌  🍌    🍌  🍌🍌🍌 "

bananas = [b1, b2, b3, b4, b5]

-- END
