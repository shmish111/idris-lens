-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Setter
import Control.Lens.Types
import Control.Monad.Identity
import Data.Profunctor

%default total

public export
sets : ((a -> b) -> s -> t) -> Setter s t a b
sets l (Mor f) = Mor $ Id . l (runIdentity . f)

public export
over : Setter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . applyMor (l (Mor (Id . f)))

public export
set : Setter s t a b -> b -> s -> t
set l b = over l (const b)

infixr 4 .~
public export
(.~) : Setter s t a b -> b -> s -> t
(.~) = set

infixr 4 &~
public export
(&~) : Setter s t a b -> (a -> b) -> s -> t
(&~) = over

-- & is a special character and can't be used
infixl 1 |>
public export
(|>) : a -> (a -> b) -> b
(|>) a f = f a

public export
mapped : Functor f => LensLike Identity (f a) (f b) a b
mapped = sets map

-- --------------------------------------------------------------------- [ EOF ]
