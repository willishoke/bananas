module Operations where

-- Polynomial representation of an equation.
-- List values are coefficients
-- [a + bx + cx² + dx³ ..]
type Equation = [Float]
type Level = Float

-- Attenuate an equation
-- Unity gain is 1.0
-- Negative values will result in inversion
attenuvert :: Equation -> Level -> Equation
attenuvert eq lvl = zipWith (*) eq $ repeat lvl

-- Integrate an equation 
integrate :: Equation -> Equation
integrate eq = 0.0 : (zipWith (/) eq [1..])

-- Differentiate an equation
differentiate :: Equation -> Equation
differentiate eq = tail $ zipWith (*) eq [0..] 

-- Multiply two equations
multiply :: Equation -> Equation -> Equation
multiply = zipWith (*)

-- Divide one equation by another
divide :: Equation -> Equation -> Equation
divide = zipWith (/) 

-- Sum a set of equations
mix :: [Equation] -> Equation
mix = foldl (zipWith (+)) (repeat 1.0)

-- Invert an equation
invert :: Equation -> Equation
invert = zipWith (*) $ repeat (-1.0)
