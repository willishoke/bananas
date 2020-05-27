{-# LANGUAGE TemplateHaskell #-}

module Module where


-- IMPORTS

import Control.Lens hiding (Const)
import qualified Data.Sequence as S


type Sample = Double


-- The value of a sample is limited to the range [-1.0, 1.0]
-- Clamp function gets applied anytime we evaluate 

clamp :: Sample -> Sample
clamp = min 1.0 . max (-1.0)


-- Module Types

data MType
  = Constant
  | Adder
  | Multiplier
  | Inverter
  | Absolute
  | Integrator
  | Differentiator
  | Delay
  | Clock
  | Output
  deriving (Show, Eq)


-- All modules share the same underlying representation.
-- Output is at the front of the buffer. 
-- Buffers may be used for other purposes: for example, the delay module 
-- uses its buffer to store prior values, and the output module holds
-- the entire output buffer.
-- The mType tag determines the evaluation function associated with a module.
-- Modules are considered equal if their mID fields match.

data Module = Module
  { _buffer :: S.Seq Sample -- Output buffer
  ,  _mType :: MType        -- Type tag
  ,    _mID :: String       -- Module ID
  }

makeLenses ''Module

instance Eq Module where
  (==) m1 m2 =
    m1^.mID == m2^.mID

instance Ord Module where
  (<=) m1 m2 =
    m1^.mID <= m2^.mID

instance Show Module where
  show m = 
       (show $ m^.mType) 
    <> " " 
    <> (m^.mID) 
    <> " = " 
    <> if null (m^.buffer) then "empty" 
         else show $ S.index (m^.buffer) 0

-- Peek at a module's output value

getValue :: Module -> Sample 
getValue m = S.index (m^.buffer) 0


-- CONSTANT

makeConst :: String -> Sample -> Module
makeConst = \name value -> Module
  { _buffer = pure $ clamp value 
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
evalAdd ss = set buffer $ pure $ clamp $ sum ss


-- MULTIPLIER

makeMul :: String -> Module
makeMul = \s -> Module
  { _buffer = pure 0.0
  ,  _mType = Multiplier
  ,    _mID = s
  }

evalMul :: [Sample] -> Module -> Module
evalMul ss = set buffer $ pure $ clamp $ product ss


-- INVERTER

makeInv :: String -> Module
makeInv = \s -> Module
  { _buffer = pure 0.0
  ,  _mType = Inverter
  ,    _mID = s
  }

evalInv :: Sample -> Module -> Module
evalInv s = set buffer $ pure $ negate s


-- ABSOLUTE VALUE 

makeAbs :: String -> Module
makeAbs = \s -> Module
  { _buffer = pure 0.0
  ,  _mType = Absolute
  ,    _mID = s
  }

evalAbs :: Sample -> Module -> Module
evalAbs s = set buffer $ pure $ abs s

-- INTEGRATOR

makeInt :: String -> Module
makeInt s = Module
  { _buffer = pure 0.0
  ,  _mType = Integrator
  ,    _mID = s
  }

evalInt :: Sample -> Module -> Module
evalInt s = over buffer $ fmap $ clamp . (+ (s/100)) 


-- Differentiator

makeDif :: String -> Module
makeDif = \s -> Module
  { _buffer = pure 0.0
  ,  _mType = Differentiator 
  ,    _mID = s
  }

evalDif :: Sample -> Module -> Module
evalDif s = over buffer $ fmap $ clamp . (s -)


-- DELAY

makeDel :: String -> Int -> Module
makeDel = \s i -> Module
  { _buffer = S.replicate i 0.0
  ,  _mType = Delay
  ,    _mID = s
  }

evalDel :: Sample -> Module -> Module
evalDel s = over buffer $ S.drop 1 . (|> s) 


-- CLOCK 

makeClk :: String -> Int -> Module
makeClk = \s i -> Module
  { _buffer = S.fromList [1.0, 0.0, fromIntegral i]
  ,  _mType = Clock
  ,    _mID = s
  }

evalClk :: Module -> Module
evalClk = over buffer $ \b ->
  let first = if (round second == 0) then negate $ S.index b 0 else S.index b 0
      second = fromIntegral $ (succ $ round $ S.index b 1) `mod` round third
      third = S.index b 2
  in S.fromList [first, second, third]


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
