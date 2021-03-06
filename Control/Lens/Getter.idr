-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Getter
import Control.Lens.Types
import Control.Lens.Const
import Data.Contravariant
import Control.Lens.First
import Data.Profunctor

%default total

public export
view : Getting a s a -> s -> a
view l = getConst . applyMor (l (Mor MkConst))

public export
views : Getting a s a -> (a -> r) -> s -> r
views l f = f . view l

public export
foldMapOf : Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst . applyMor (l (Mor (MkConst . f)))

-- Creates a lens where the Functor instance must be
-- covariant. Practically this means we can only use
-- Const, so this is a valid getter and nothing else

||| Create a Getter from arbitrary functions `s -> a`.
public export
to : Contravariant f => (s -> a) -> LensLike' f s a
to k = dimap k (contramap k)

infixl 8 ^.
public export
(^.) : s -> Getting a s a -> a
a ^. l = view l a

infixl 8 ^?
public export
(^?) : s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (MkFirst . Just) s)

-- --------------------------------------------------------------------- [ EOF ]
