-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Const
import Data.Contravariant

||| Const Functor.
public export
data Const : (0 a : Type) -> (b : Type) -> Type where
  MkConst: a -> Const a b

public export
getConst : Const a b -> a
getConst (MkConst x) = x

public export
Functor (Const a) where
  map _ (MkConst x) = (MkConst x)

public export
Contravariant (Const a) where
  contramap _ (MkConst x) = (MkConst x)

public export
implementation Monoid m => Applicative (Const m) where
  pure _                      = MkConst neutral
  (MkConst f) <*> (MkConst v) = MkConst (f <+> v)


-- --------------------------------------------------------------------- [ EOF ]
