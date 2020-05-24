{-# LANGUAGE TemplateHaskell #-}

module Module where

import Control.Lens hiding (Const)
import qualified Data.Sequence as S

-- The value of a sample is limited to the range [-1.0, 1.0]
type Sample = Float 

-- Module Types
data MType
  = Constant
  | Adder
  | Multiplier
  | Inverter
  | Integrator
  | Differentiator
  | Delay
  | Output
  deriving (Show, Eq)

data Module = Module
  { _buffer :: S.Seq Sample -- Output buffer
  ,  _mType :: MType        -- Type tag
  ,    _mID :: String       -- Module ID
  }
  deriving (Show)

makeLenses ''Module

-- Modules are considered equal if their ID's match
instance Eq Module where
  (==) m1 m2 =
    m1^.mID == m2^.mid


-- Change state of a module on input
eval :: [Sample] -> Module -> Module
eval ss m
  | null ss = m -- No incoming connections
  | otherwise = case m^.mType of
      Constant        -> m
      Adder           -> evalAdd ss m 
      Multiplier      -> evalMul ss m 
      Inverter        -> evalInv (head ss) m
      Integrator      -> evalIntegrator (head ss) m
      Differentiator  -> evalDiff (head ss) m
      Delay           -> evalDel (head ss) m
      Output          -> evalOut (head ss) m


-- Peek at a module's output value
getValue :: Module -> Sample 
getValue m = S.index (m^.buffer) 0


-- CONSTANT
makeConst :: String -> Sample -> Module
makeConst = \name value -> Module
  { _buffer = pure value 
  ,  _mType = Constant
  ,    _mID = name
  }


-- ADDER
makeAdd :: String -> Module
makeAdd = \s -> Module
  { _buffer = pure 0.0
  ,  _mType = Adder 
  ,    _mID = s
  }

evalAdd :: [Sample] -> Module -> Module
evalAdd ss = set buffer $ pure $ sum ss


-- MULTIPLIER
makeMul :: String -> Module
makeMul = \s -> Module
  { _buffer = pure 0.0
  ,  _mType = Multiplier
  ,    _mID = s
  }

evalMul :: [Sample] -> Module -> Module
evalMul ss = set buffer $ pure $ product ss


-- INVERTER
makeInv :: String -> Module
makeInv = \s -> Module
  { _buffer = pure 0.0
  ,  _mType = Inverter
  ,    _mID = s
  }

evalInv :: Sample -> Module -> Module
evalInv s = set buffer $ pure $ negate s


-- INTEGRATOR
makeIntegrator :: String -> Module
makeIntegrator s = Module
  { _buffer = pure 0.0
  ,  _mType = Integrator
  ,    _mID = s
  }

evalIntegrator :: Sample -> Module -> Module
evalIntegrator s = over buffer $ fmap (s +) 


-- Differentiator

makeDiff :: String -> Module
makeDiff = \s -> Module
  { _buffer = pure 0.0
  ,  _mType = Differentiator 
  ,    _mID = s
  }

evalDiff :: Sample -> Module -> Module
evalDiff s = over buffer $ fmap (s -)


-- DELAY
makeDel :: String -> Int -> Module
makeDel = \s i -> Module
  { _buffer = S.replicate i 0.0
  ,  _mType = Delay
  ,    _mID = s
  }

evalDel :: Sample -> Module -> Module
evalDel s = over buffer $ S.drop 1 . (|> s) 


-- OUTPUT
makeOut :: String -> Module
makeOut = \s -> Module
  { _buffer = S.empty
  ,  _mType = Output
  ,    _mID = s
  }

evalOut :: Sample -> Module -> Module
evalOut s = over buffer $ (|> s)


-- END
