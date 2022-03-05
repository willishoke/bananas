{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Parser
import Module

import Prelude hiding (readFile, lines)
import Data.Char (isDigit)
import Data.List (transpose)
import Data.Text (Text, lines)
import Data.Text.IO (readFile)
import Algebra.Graph.AdjacencyMap hiding (transpose)
import System.Environment
import Control.Monad.State
import Control.Lens hiding (transform)
import Data.Foldable (toList)
import Data.WAVE
import Brick

import qualified Data.Sequence as S


--  🍌🍌🍌      🍌     🍌    🍌     🍌     🍌    🍌     🍌     🍌🍌🍌
--  🍌   🍌    🍌🍌    🍌🍌  🍌    🍌🍌    🍌🍌  🍌    🍌🍌   🍌      
--  🍌🍌🍌    🍌🍌🍌   🍌 🍌 🍌   🍌🍌🍌   🍌 🍌 🍌   🍌🍌🍌   🍌🍌🍌 
--  🍌   🍌  🍌    🍌  🍌  🍌🍌  🍌    🍌  🍌  🍌🍌  🍌    🍌       🍌
--  🍌🍌🍌   🍌    🍌  🍌    🍌  🍌    🍌  🍌    🍌  🍌    🍌  🍌🍌🍌 

-- A LANGUAGE FOR SIMULATION OF ANALOG COMPUTATION


-- parsed command line args

data Config = Config
  { _infile :: String
  , _outfile :: String
  , _sampleCount :: Int
  }

makeLenses ''Config

ui :: Widget ()
ui = str "Hello, world!"


data Test = Test { a :: String, b :: String } deriving (Show, Eq, Ord)

main :: IO ()
main = simpleMain ui

-- MAIN
{--
main :: IO ()
main = do
  runStateT runComp initState
  return ()

--}
-- check to make sure command line args are well-formed

parseArgs :: IO Config
parseArgs = do
  args <- getArgs
  when (length args < 3 || not (all isDigit $ args !! 2)) $
    error "Usage: stack run <filename.nana> <output.wave> <iterations>" 
  pure $ Config
    { _infile = args !! 0
    , _outfile = "waveforms/" <> args !! 1
    , _sampleCount = read $ args !! 2
    } 


-- right now there isn't much point in having state stick around,
-- but if the compiler were interactive, this would be useful

type Program = StateT CompState IO ()

runComp :: Program
runComp = do
  config <- liftIO parseArgs
  pText <- liftIO $ readFile $ config^.infile
  pState <- get
  let seed = parseLines (lines pText) pState 
      computed = comp (config^.sampleCount) seed
      sound = getOutput computed
  liftIO $ toWave (config^.outfile) sound
  return ()


-- perform all steps of computation

comp :: Int -> CompState -> CompState 
comp i c
  | i <= 0 = c
  | otherwise = comp (pred i) (step c)


-- perform a single step of computation over the ASG

step :: CompState -> CompState
step = over network transform

-- first step is to grab the postset of each element
-- convert to list, then grab all the buffers
-- find their first elements. these get passed along to be evaluated
-- thanks to pointfree.io for helping remove all the extra lambdas

transform :: AdjacencyMap Module -> AdjacencyMap Module
transform ms =
  flip gmap ms $ g . f >>= eval where
    f = toList . flip postSet ms
    g = map $ flip S.index 0 . (^.buffer)


-- grab all samples from the output buffers and convert to list
-- vector would be more efficient but WAVE expects lists
     
getOutput :: CompState -> [[Sample]]
getOutput c =
  let ms = vertexList (c^.network)
      outs = filter (\v -> v^.mType == Output) ms
  in transpose $ map (toList . (^.buffer)) outs


-- generate a .wav file from raw data

toWave :: String -> [[Sample]] -> IO ()
toWave = \filename samples ->
  let raw = map (map $ doubleToSample . (/2.0)) samples
      h = WAVEHeader
        { waveNumChannels = length $ head raw
        , waveFrameRate = sampleRate
        , waveBitsPerSample = 16
        , waveFrames = Just $ length raw
        }
      w = WAVE { waveHeader = h, waveSamples = raw }
  in putWAVEFile filename w


-- change state of a module on input

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
  Sine            -> if null ss then evalSin 1.0 m else evalSin (head ss) m
  Output          -> if null ss then m else evalOut (head ss) m


-- END
