-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Data.Contravariant

public export
interface Contravariant (0 f : Type -> Type) where
  contramap : (b -> a) -> f a -> f b

-- --------------------------------------------------------------------- [ EOF ]
