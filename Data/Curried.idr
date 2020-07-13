-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Data.Curried

import Data.Morphisms

public export
data Curried : ( g : Type -> Type ) -> ( h : Type -> Type ) -> ( a : Type ) -> Type where
  MkCurried : (forall r. g (a -> r) -> h r) -> Curried g h a

public export
Functor g => Functor (Curried g h) where
  map f (MkCurried g) = MkCurried (g . map (. f))

public export
Functor g => Applicative (Curried g g) where
  pure a = MkCurried (map (\f => f a))
  (MkCurried mf) <*> (MkCurried ma) = MkCurried (ma . mf . map (.))

public export
liftCurried : Applicative f => f a -> Curried f f a
liftCurried fa = MkCurried (<*> fa)

public export
lowerCurried : Applicative f => Curried f g a -> g a
lowerCurried (MkCurried f) = f (pure id)
