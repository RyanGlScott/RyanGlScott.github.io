-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
module GAGG where

import Data.Kind

-- | Class of types whose values can be reduced to normal form.
class NFData a where
  rnf :: a -> ()

instance NFData Int where
  rnf = (`seq` ())

instance NFData Bool where
  rnf = (`seq` ())

class Generic a where
  type Rep a
  from :: a -> Rep a
  to :: Rep a -> a

data U1 = U1

newtype K1 c = K1 c

infixr 6 :*:
data a :*: b = a :*: b

infixr 5 :+:
data a :+: b = L1 a | R1 b

-- Example instance
instance Generic [a] where
  type Rep [a] = U1                -- [] constructor
             :+: (K1 a :*: K1 [a]) -- (:) constructor
  from []     = L1 U1
  from (x:xs) = R1 (K1 x :*: K1 xs)
  to (L1 U1)               = []
  to (R1 (K1 x :*: K1 xs)) = x:xs

genericRNF :: (Generic a, NFData (Rep a)) => a -> ()
genericRNF = rnf . from

instance NFData U1 where
  rnf U1 = ()

instance NFData c => NFData (K1 c) where
  rnf (K1 c) = rnf c

instance (NFData a, NFData b) => NFData (a :*: b) where
  rnf (x :*: y) = rnf x `seq` rnf y

instance (NFData a, NFData b) => NFData (a :+: b) where
  rnf (L1 x) = rnf x
  rnf (R1 y) = rnf y

data ADTExample a
  = ADTEx1 Int
  | ADTEx2 Bool a
  | ADTEx3
  -- deriving Generic

instance Generic (ADTExample a) where
  type Rep (ADTExample a) =     K1 Int
                            :+: (K1 Bool :*: K1 a)
                            :+: U1

  from (ADTEx1 x)   = L1 (K1 x)
  from (ADTEx2 x y) = R1 (L1 (K1 x :*: K1 y))
  from ADTEx3       = R1 (R1 U1)

  to (L1 (K1 x))               = ADTEx1 x
  to (R1 (L1 (K1 x :*: K1 y))) = ADTEx2 x y
  to (R1 (R1 U1))              = ADTEx3

instance NFData a => NFData (ADTExample a) where
  rnf = genericRNF

instance NFData a => NFData [a] where
  rnf = genericRNF

{-
-- Now, how to extend this to GADTs:

data GADTExample :: Type -> Type -> Type where
  GADTEx1 :: NFData a => a -> GADTExample a b
  GADTEx2 :: NFData b => b -> GADTExample a b
-}

{-
-- We'd like to generically derive this:
instance NFData (GADTExample a b) where
  rnf (GADTEx1 x) = rnf x
  rnf (GADTEx2 y) = rnf y
-}

{-
-- Attempt 1:
instance (NFData a, NFData b) => Generic (GADTExample a b) where
  type Rep (GADTExample a b) = K1 a :+: K1 b
  from (GADTEx1 x) = L1 (K1 x)
  from (GADTEx2 y) = R1 (K1 y)
  to (L1 (K1 x)) = GADTEx1 x
  to (R1 (K1 y)) = GADTEx2 y

-- Blegh, the constraints have percolated to the top:
instance (NFData a, NFData b) => NFData (GADTExample a b) where
  rnf = genericRNF
-}

-- Better attempt: use a new representation type!

data ExConstr :: Constraint -> Type -> Type where
  ExConstr :: c => x -> ExConstr c x

instance (c => NFData x) => NFData (ExConstr c x) where
  rnf (ExConstr x) = rnf x

{-
instance Generic (GADTExample a b) where
  type Rep (GADTExample a b) =     ExConstr (NFData a) (K1 a)
                               :+: ExConstr (NFData b) (K1 b)
  from (GADTEx1 x) = L1 (ExConstr (K1 x))
  from (GADTEx2 y) = R1 (ExConstr (K1 y))
  to (L1 (ExConstr (K1 x))) = GADTEx1 x
  to (R1 (ExConstr (K1 y))) = GADTEx2 y

instance NFData (GADTExample a b) where
  rnf = genericRNF
-}

-- We can even add equalities
data GADTExample :: Type -> Type -> Type where
  GADTEx1 :: NFData a => a -> GADTExample a b
  GADTEx2 :: NFData b => b -> GADTExample a b
  GADTEx3 ::   Int -> Bool -> GADTExample Int Bool

{-
-- How do we represent these? Note that in Core, this data type looks closer
-- to something like this:
data GADTExample :: Type -> Type -> Type where
  GADTEx1 :: NFData a            => a           -> GADTExample a b
  GADTEx2 :: NFData b            => b           -> GADTExample a b
  GADTEx3 :: (a ~ Int, b ~ Bool) => Int -> Bool -> GADTExample a b
-- So let's just use this encoding!
-}

instance Generic (GADTExample a b) where
  type Rep (GADTExample a b) =     ExConstr (NFData a) (K1 a)
                               :+: ExConstr (NFData b) (K1 b)
                               :+: ExConstr (a ~ Int, b ~ Bool) (K1 Int :*: K1 Bool)
  from (GADTEx1 x)   = L1 (ExConstr (K1 x))
  from (GADTEx2 y)   = R1 (L1 (ExConstr (K1 y)))
  from (GADTEx3 i b) = R1 (R1 (ExConstr (K1 i :*: K1 b)))
  to (L1 (ExConstr (K1 x)))               = GADTEx1 x
  to (R1 (L1 (ExConstr (K1 y))))          = GADTEx2 y
  to (R1 (R1 (ExConstr (K1 i :*: K1 b)))) = GADTEx3 i b

instance NFData (GADTExample a b) where
  rnf = genericRNF

-----
-- Enter existential quantification
-----

-- How do we represent something like this?
data SomeNFThing :: Type where
  SomeNFThing :: forall a. NFData a => a -> SomeNFThing
-- Unlike in previous examples, we now have an /existentially quantified/ type
-- variable `a` in a constructor. How do we generically represent existential
-- quantification?

{-
-- Ideally, we'd like to be able to write something like this:
instance Generic SomeNFThing where
  type Rep SomeNFThing = ExQuant (\a -> ExConstr (NFData a) (K1 a))
                                 -- ^ Type-level lambda
  ...
-- But there's a problem: there's no type-level lambdas in GHC. But what if
-- we could convincingly fake them?
-}

{-
-- Here's one idea:
type RepAux (a :: Type) = ExConstr (NFData a) (K1 a)

instance Generic SomeNFThing where
  type Rep SomeNFThing = ExQuant RepAux

-- But this still won't work: you can't partially apply type synonyms like this.
-- But what if we could convincingly fake partial application of type synonyms?
-- /This/ we can do, with...
-}

-- Defunctionalization!
{-
data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
-}
type a ~> b = a -> b -> Type
infixr 0 ~>
type family Apply (f :: k1 ~> k2) (x :: k1) :: k2

-- We'd like to imagine RepAux as having the kind:
-- type RepAux :: Type ~> Type
type RepAux (a :: Type) = ExConstr (NFData a) (K1 a)

-- In pursuit of this vision, we will define a /defunctionalization symbol/
-- for RepAux.
data RepAuxSym :: Type ~> Type
-- The Apply type family dispatches on defunctionalization symbols, and applies
-- arguments in the way you'd expect it to.
type instance Apply RepAuxSym a = RepAux a

{-
-- With defunctionalization, we /can/ write:
instance Generic SomeNFThing where
  type Rep SomeNFThing = ExQuant RepAuxSym
                         -- Explicitly indicate partial application of RepAux
                         -- by using its defunctionalization symbol
-- Cool! So how do we define this ExQuant representation type?
-}

{-
-- First attempt:
data ExQuant :: (Type ~> Type) -> Type where
  ExQuant :: forall f x. -- That x is existentially quantified
             Apply f x -> ExQuant f

instance Generic SomeNFThing where
  type Rep SomeNFThing = ExQuant RepAuxSym
  from (SomeNFThing x) = ExQuant (ExConstr (K1 x))
  to (ExQuant (ExConstr (K1 x))) = SomeNFThing x

-- Let's try defining an NFData instance for ExQuant, using QuantifiedConstraints:
instance (forall x. NFData (Apply f x)) => NFData (ExQuant f) where
  rnf (ExQuant x) = rnf x

-- Sadly, this won't quite work:

    GAGG.hs:228:10: error:
        • Illegal type synonym family application in instance: Apply f x
        • In the quantified constraint ‘forall x. NFData (Apply f x)’
          In the instance declaration for ‘NFData (ExQuant f)’

-- Alas, quantified constraints are "local instances", and you can't define
-- instances for type families. Major bummer.
-}

-- But all is not lost! We will use a trick passed down from the GHC sages:
-- when in doubt, use another newtype:
newtype WrappedApply :: (Type ~> Type) -> (Type -> Type) where
  WrapApply :: Apply f x -> WrappedApply f x

instance NFData (Apply f x) => NFData (WrappedApply f x) where
  rnf (WrapApply x) = rnf x

data ExQuant :: (Type ~> Type) -> Type where
  ExQuant :: forall f x. -- That x is existentially quantified!
             WrappedApply f x -> ExQuant f

instance Generic SomeNFThing where
  type Rep SomeNFThing = ExQuant RepAuxSym
  from (SomeNFThing x) = ExQuant (WrapApply (ExConstr (K1 x)))
  to (ExQuant (WrapApply (ExConstr (K1 x)))) = SomeNFThing x

-- There's no issue with having a quantified constraint for a newtype!
instance (forall x. NFData (WrappedApply f x)) => NFData (ExQuant f) where
  rnf (ExQuant x) = rnf x

-- Now we can reap the benefits:
instance NFData SomeNFThing where
  rnf = genericRNF
