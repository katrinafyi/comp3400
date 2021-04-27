module ComplexNumbers
(Complex,
 conjugate,
 abs,
 exp,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)

import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Complex a = Complex { getReal :: a, getImag :: a } deriving (Eq, Show)

instance Functor Complex where
  fmap f (Complex re im) = Complex (f re) (f im)

complex :: (a, a) -> Complex a
complex = uncurry Complex

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex re im) = Complex re (-im)

abs :: Floating a => Complex a -> a
abs (Complex re im) = sqrt $ re^2 + im^2

real :: Num a => Complex a -> a
real = getReal

imaginary :: Num a => Complex a -> a
imaginary = getImag

exp :: Floating a => Complex a -> Complex a
exp (Complex a b) = (P.exp a *) <$> Complex (cos b) (sin b)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
Complex a b `mul` Complex c d = Complex re im
  where re = a*c - b*d
        im = b*c + a*d

add :: Num a => Complex a -> Complex a -> Complex a
Complex a b `add` Complex c d = Complex (a+c) (b+d)

sub :: Num a => Complex a -> Complex a -> Complex a
a `sub` b = a `add` fmap negate b

div :: Fractional a => Complex a -> Complex a -> Complex a
Complex a b `div` Complex c d = Complex re im
  where re = (a*c + b*d) / divider
        im = (b*c - a*d) / divider
        divider = c^2 + d^2
