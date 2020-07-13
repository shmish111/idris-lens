-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.First

public export
record First (a : Type) where
  constructor MkFirst
  getFirst : Maybe a

public export
Semigroup (First a) where
  (MkFirst f) <+> r = case f of
                        Nothing => r
                        Just x  => MkFirst f

public export
Monoid (First a) where
  neutral = MkFirst Nothing
