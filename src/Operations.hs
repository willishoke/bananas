module Operations where

-- import Data.Vector
import Data.Word
import Control.Arrow
import Algebra.Graph
import Codec.Audio.Wave
import Control.Monad.State

-- Poly representation of an equation.
-- List values are coefficients

--   [a + bx + cx² + dx³ ..]
-- * [h + ix + jx² + kx³ ..]

type Poly = [Float]
type Level = Float
type Program = StateT (Graph Poly) IO ()

main :: IO ()
main = do 
  runStateT code empty
  return ()

-- liftIO $ print "hello" to do IO stuff
 -- (x:xs) <- get
  -- put xs
  -- return x
code :: Program
code = do
  liftIO $ print "Hello"
  return ()


linear = [0.0, 1.0]
quadratic = [0.0, 0.0, 1.0]
cubic = [0.0, 0,0, 0,0, 1.0]
sine = [0.0, 1.0, 0.0, -(1.0/6.0), 0.0, (1.0/120.0), 0.0, -(1.0/5040.0), 0.0, (1.0/362880.0)] :: [Float]

to16bit :: Float -> Word16
to16bit = fromIntegral . floor . (*(2^16-1)) . (/2.0) . (+1.0)

printPoly :: Poly -> String
printPoly = init . init . init
          . concat
          . reverse
          . map (++ " + ")
          . zipWith (flip (++)) (map show [0..])
          . map (++ "x^")
          . map show

-- Clamp a number between two values
clamp :: (Num a, Ord a) => a -> a -> a -> a 
clamp lo hi = max lo . min hi

-- Evaluate a polynomial
evaluate :: Poly -> Float -> Float
evaluate eq x =
  let powers = zipWith (^) (repeat x) [0..]
  in sum $ zipWith (*) eq powers

-- Integrate a polynomial
integrate :: Poly -> Poly
integrate eq = 0.0 : (zipWith (/) eq [1..])

-- Differentiate a polynomial
differentiate :: Poly -> Poly
differentiate eq = tail $ zipWith (*) eq [0..] 

-- Integrate a polynomial
-- Attenuate a polynomial
-- Unity gain is 1.0
-- Negative values will result in inversion
attenuvert :: Level -> Poly -> Poly
attenuvert level poly = zipWith (*) poly $ repeat level


-- Polynomial multiplication algorithm

--   [a + bx + cx²]
-- * [α + βx + γx²]
----------------------------
--   [aα + aβx + aγx² + bαx + bβx² + bγx³ + cαx² + cβx³ + cγx⁴]
-- = [aα + (aβ + bα)x + (aγ + bβ + cα)x² + (bγ + cβ)x³ + cγx⁴]

--   multiply [a, b, c] 
--            [α, β, γ]

-- = [aα, aβ + bα, aγ + bβ + cα, bγ + cβ, cγ]

-- = zipWith3 (+) [aα, aβ, aγ,  0,  0]
--                [0,  bα, bβ, bγ,  0]
--                [0,   0, cα, cβ, cγ]

--   [a + bx]
-- * [α + βx + γx² + δx³] 
----------------------------
--   [aα + aβx + aγx² + aδx³ + bαx + bβx² + bγx³ + bδx⁴]
-- = [aα + (aβ + bα)x + (aγ + bβ)x² + (aδ + bγ)x³ + bδx⁴]

--   multiply [a, b] 
--            [α, β, γ, δ]

-- = [aα, aβ + bα, aγ + bβ, aδ + bγ, bδ]

-- = zipWith (+) [aα, aβ, aγ, aδ,  0]
--               [ 0, bα, bβ, bγ, bδ]

--   multiply [a, b, c, d]
--            [α, β]

-- = [aα, aβ + bα, bβ + cα, cβ + dα, dβ]

-- = zipWith (+) [aα, aβ,  0,  0,  0]
--               [ 0, bα, bβ,  0,  0]
--               [ 0,  0, cα, cβ,  0]
--               [ 0,  0,  0, dα, dβ]

-- In general, it is computationally more efficient to
-- multiply a a shorter polynomial by a longer polynomial

multiply :: Poly -> Poly -> Poly
multiply p1 p2 =
  let maps = map (\x -> map (*x) p2) p1
      offsetBy = \n -> (++) (replicate n 0)
      zips = zipWith offsetBy [0..] maps
  in foldl add (pure 0) zips

-- Divide one polynomial by another
-- Use synthetic division? What to do in case of uneven division?
divide :: Poly -> Poly -> Poly
divide = undefined

zipWithPad :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
zipWithPad f idElem = go
  where
    go [] [] = []
    go (x:xs) [] = f x idElem : go xs []
    go [] (y:ys) = f y idElem : go [] ys
    go (x:xs) (y:ys) = f x y : go xs ys

add :: Poly -> Poly -> Poly
add = zipWithPad (+) 0.0

-- Average two polynomials
average :: Poly -> Poly -> Poly
average x y = attenuvert 0.5 $ add x y

-- Invert a polynomial 
invert :: Poly -> Poly
invert = zipWith (*) $ repeat (-1.0)

