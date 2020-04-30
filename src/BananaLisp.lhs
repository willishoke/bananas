> {-# LANGUAGE OverloadedStrings #-}

> module BananaLisp where

> import Data.Word
> import Data.Text (Text)
> import Control.Arrow
> import Algebra.Graph
> import Control.Monad.State
> import Codec.Audio.Wave
> import Data.Attoparsec.Text
> import Control.Applicative ((<|>))
> import qualified Data.Map.Lazy as Map


Time flows in discrete units.

> type Time = Int


Poly representation of an equation.
List values are coefficients:
  [a + bx + cx² + dx³ ..]

> newtype Poly = Poly { poly :: [Float] }

For now: a signal is just a list of floating point values.

> type Signal = [Float]


For now: A stream is a map from identifiers to signals.

> type Stream = [Map.Map Char Signal] 
> type Level = Float


This represents the state of our program.

> data CompState = CompState
>   { network :: Graph Poly
>   , inputStream :: Signal
>   , outputStream :: Signal
>   , namedInputs :: Stream
>   , namedOutputs :: Stream
>   }

> initState :: CompState
> initState = CompState
>   { network = empty
>   , inputStream = []
>   , outputStream = []
>   , namedInputs = []
>   , namedOutputs = []
>   }

> type Program = StateT CompState IO ()

There are multiple input streams:
"Signal" and delay lines
Delay lines populated from modules
Signals can be input to a module
Modules can be connected to other modules or to an output stream

In global state we should track:
  - Each stream
  - Current graph

Each "tick" we:
  - Update graph
  - Consume data from input stream(s)
    First consume main input
    Then aux input
 - Send data to output stream(s)
    This can happen in any order

> main :: IO ()
> main = do 
>   runStateT code initState
>   return ()

liftIO $ print "hello" -- to do IO stuff

stack example:
  (x:xs) <- get
  put xs
  return x

> code :: Program
> code = do
>   liftIO $ print "Hello"
>   return ()

Given a file name, attempt to get the contents
of the file.

> readBLISP :: String -> IO (Graph Poly)
> readBLISP fileName = do
>   contents <- readFile fileName
>   pure empty -- call parser here

> polyUnaryParser :: Parser (Poly -> Poly)
> polyUnaryParser =
>       (string "I" >> pure integrate)
>   <|> (string "D" >> pure differentiate)

> polyBinaryParser :: Parser (Poly -> Poly -> Poly)
> polyBinaryParser =
>       (string "A" >> pure add)
>   <|> (string "M" >> pure multiply)


> linear = Poly [0.0, 1.0]
> quadratic = Poly [0.0, 0.0, 1.0]
> cubic = Poly [0.0, 0,0, 0,0, 1.0]
> sine = Poly [0.0, 1.0, 0.0, -(1.0/6.0), 0.0, (1.0/120.0), 0.0, -(1.0/5040.0), 0.0, (1.0/362880.0)]

> to16bit :: Float -> Word16
> to16bit = fromIntegral
>         . floor
>         . (*(2^16-1))
>         . (/2.0)
>         . (+1.0)

We could make Poly an instance of the Show typeclass,
but since it's just a type synonym this suffices.

> printPoly :: Poly -> String
> printPoly = init . init . init
>           . concat
>           . reverse
>           . map (++ " + ")
>           . zipWith (flip (++)) (map show [0..])
>           . map (++ "x^")
>           . map show
>           . poly

Clamp a number between two values

> clamp :: (Num a, Ord a) => a -> a -> a -> a 
> clamp lo hi = max lo . min hi


Evaluate a polynomial for a particular value of x

> evaluate :: Poly -> Float -> Float
> evaluate eq x =
>   let powers = zipWith (^) (repeat x) [0..]
>   in sum $ zipWith (*) (poly eq) powers

Integrate a polynomial

> integrate :: Poly -> Poly
> integrate eq = Poly $ 0.0 : (zipWith (/) (poly eq) [1..])

Differentiate a polynomial

> differentiate :: Poly -> Poly
> differentiate eq = Poly $ tail $ zipWith (*) (poly eq) [0..] 

Attenuate a polynomial
Unity gain is 1.0
Negative values will result in inversion

> attenuate :: Level -> Poly -> Poly
> attenuate level p = Poly $ zipWith (*) (poly p) $ repeat level


Polynomial multiplication algorithm

  [a + bx + cx²]
* [α + βx + γx²]
--------------------------
  [aα + aβx + aγx² + bαx + bβx² + bγx³ + cαx² + cβx³ + cγx⁴]
= [aα + (aβ + bα)x + (aγ + bβ + cα)x² + (bγ + cβ)x³ + cγx⁴]

multiply [a, b, c] 
         [α, β, γ]

= [aα, aβ + bα, aγ + bβ + cα, bγ + cβ, cγ]

= zipWith3 (+) [aα, aβ, aγ,  0,  0]
               [0,  bα, bβ, bγ,  0]
               [0,   0, cα, cβ, cγ]

  [a + bx]
* [α + βx + γx² + δx³] 
--------------------------
  [aα + aβx + aγx² + aδx³ + bαx + bβx² + bγx³ + bδx⁴]
= [aα + (aβ + bα)x + (aγ + bβ)x² + (aδ + bγ)x³ + bδx⁴]

multiply [a, b] 
[α, β, γ, δ]

= [aα, aβ + bα, aγ + bβ, aδ + bγ, bδ]

= zipWith (+) [aα, aβ, aγ, aδ,  0]
              [ 0, bα, bβ, bγ, bδ]

multiply [a, b, c, d]
[α, β]

= [aα, aβ + bα, bβ + cα, cβ + dα, dβ]

= fold (zipWith (+)) [0] 
    [aα, aβ,  0,  0,  0]
    [ 0, bα, bβ,  0,  0]
    [ 0,  0, cα, cβ,  0]
    [ 0,  0,  0, dα, dβ]

In general, it is computationally more efficient to
multiply a a shorter polynomial by a longer polynomial

> multiply p1 p2 =
>   let pairs = map (\x -> map (*x) (poly p2)) (poly p1)
>       offsetBy = \n -> (++) (replicate n 0)
>       zips = map Poly $ zipWith offsetBy [0..] pairs
>   in foldl add (Poly $ pure 0) zips


This is just zip, but the shorter list will be padded
  to match the length of the longer list.
An identity element is given to allow for padding.
Obviously this function should not be used on infinite lists.

> zipWithPad :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
> zipWithPad f idElem = go
>   where
>     go [] [] = []
>     go (x:xs) [] = f x idElem : go xs []
>     go [] (y:ys) = f y idElem : go [] ys
>     go (x:xs) (y:ys) = f x y : go xs ys

Simple sum function for polynomials. 

> add :: Poly -> Poly -> Poly
> add p1 p2 = Poly $ zipWithPad (+) 0.0 (poly p1) (poly p2)

Average two polynomials by adding together and attenuating.

> average :: Poly -> Poly -> Poly
> average x y = attenuate 0.5 $ add x y

Invert a polynomial
This just changes the sign of all terms.

> invert :: Poly -> Poly
> invert p = Poly $ zipWith (*) (repeat (-1.0)) (poly p)

END
