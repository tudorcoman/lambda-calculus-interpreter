module NatClass where

import Prelude (Show(..), (<>), Num(fromInteger), undefined)
import qualified GHC.Natural as Natural

import MyPrelude
import BoolClass
import MaybeClass
import PairClass
 
-- | The class of Natural-like types (types having a notion of
-- 'zero', 'succ'essor, and 'iter'ation).
-- Instances should satisfy the following:
--
-- [Zero Iterations] @'iter' f i 'zero' = i@
-- [Successor Iterations]  @'iter' f i ('succ' n) = f ('iter' f i n)@
class NatClass n where
  zero :: n
  succ :: n -> n
  iter :: (a -> a) -> a -> n -> a

instance NatClass Natural.Natural where
  zero = 0
  succ n = n `Natural.plusNatural` 1
  iter f i 0 = i
  iter f i n = f (iter f i (n `Natural.minusNatural` 1))

one :: NatClass n => n
one = succ zero

-- >>> one :: Natural.Natural
-- 1

-- | The isZero function returns 'true' iff its argument is 'zero'.
isZero :: NatClass n => n -> CBool
isZero n = iter (const false) true n

-- >>> isZero (one :: Natural.Natural)
-- CFalse

-- | Usual natural numbers addition
add :: NatClass n => n -> n -> n
add a b = iter succ a b

-- >>> add one one :: Natural.Natural
-- 2

-- | Usual natural numbers multiplication
mul :: NatClass n => n -> n -> n
mul a b = iter (add a) zero b

-- >>> mul one one :: Natural.Natural
-- 1

-- | Usual natural numbers exponentiation (@exp m n@ is @m ^ n@)
exp :: NatClass n => n -> n -> n
exp a b = iter (mul a) one b

-- >>> exp (add one one) (add one one) :: Natural.Natural
-- 4

psi :: (PairClass p, NatClass n) => p n n -> p n n 
psi p = ite (isZero (fst p)) (pair (add one (fst p)) (snd p)) (pair (add one (fst p)) (add one (snd p)))

pred_aux :: NatClass n => n -> CPair n n 
pred_aux n = iter psi (pair zero zero) n

-- | Predecessor of a natural number ('nothing' for 'zero')
pred :: NatClass n => n -> CMaybe n
pred n = ite (isZero n) nothing (just (snd (pred_aux n)))

-- >>> pred zero :: CMaybe Natural.Natural
-- CNothing

-- | Difference between natural numbers as a 'MaybeClass' ('nothing' if first is smaller)
sub :: NatClass n  => n -> n -> CMaybe n
sub a b = ite (isZero a) (ite (isZero b) (just zero) nothing) (ite (isZero b) (just a) (sub (fromMaybe zero (pred a)) (fromMaybe zero (pred b))))

-- >>> sub (exp (add one one) (add one one)) one :: CMaybe Natural.Natural
-- CJust 3

-- | (Strictly-)Less-Than predicate for natural numbers.
lt :: NatClass n => n -> n -> CBool 
lt a b = isNothing (sub a b) 

-- >>> lt (one :: Natural.Natural) one
-- CFalse

-- | (Strictly-)Greater-Than predicate for natural numbers.
gt :: NatClass n => n -> n -> CBool 
gt a b = isNothing (sub b a)

-- >>> gt (one :: Natural.Natural) one
-- CFalse

-- | Greater-Than-or-Equal-To predicate for natural numbers.
gte :: NatClass n => n -> n -> CBool 
gte a b = isZero (fromMaybe zero (sub b a))

-- >>> gte (zero :: Natural.Natural) zero
-- CTrue

-- | Less-Than-or-Equal-To predicate for natural numbers.
lte :: NatClass n => n -> n -> CBool 
lte a b = isZero (fromMaybe zero (sub a b))

-- >>> lte (zero :: Natural.Natural) zero
-- CTrue

-- | Equality predicate for natural numbers.
eq :: NatClass n => n -> n -> CBool 
eq a b = (lte a b) && (gte a b)

-- >>> eq (zero :: Natural.Natural) zero
-- CTrue

-- >>> eq (zero :: Natural.Natural) one
-- CFalse

-- | Returns the greater between its two arguments
max :: NatClass n => n -> n -> n
max a b = ite (gte a b) a b

-- >>> max (zero :: Natural.Natural) one
-- 1

newtype CNat = CNat { getCNat :: forall a . (a -> a) -> a -> a }

instance NatClass CNat where
  iter f i n = getCNat n f i
  zero = CNat (\_ i -> i)
  succ n = CNat (\f i -> f (getCNat n f i))

-- | converting between different instances of 'NatClass'
fromNatClass :: (NatClass n, NatClass m) => n -> m
fromNatClass = iter succ zero

-- | 'Show' instance for 'CNat' (via transformation into Haskell Natural)
instance Show CNat where
  show cn = "C" <> show (fromNatClass cn :: Natural.Natural)

-- | to allow recognizing natural numbers as 'CNat'
instance Num CNat where
  fromInteger n = fromNatClass (fromInteger n :: Natural.Natural)

-- >>> 1 :: CNat
-- C1

-- >>> isZero (1 :: CNat)
-- CFalse

-- >>> add 3 5 :: CNat
-- C8

-- >>> mul 2 7 :: CNat
-- C14

-- >>> exp 2 10 :: CNat
-- C1024

-- >>> pred 0 :: CMaybe CNat
-- CNothing

-- >>> sub 5 5 :: CMaybe CNat
-- CJust C0

-- >>> lt 7 (7 :: CNat)
-- CFalse

-- >>> gt (one :: CNat) one
-- CFalse

-- >>> gte (zero :: CNat) zero
-- CTrue

-- >>> lte (zero :: CNat) zero
-- CTrue

-- >>> eq (zero :: CNat) zero
-- CTrue

-- >>> eq (zero :: CNat) one
-- CFalse

-- >>> max (zero :: CNat) one
-- C1