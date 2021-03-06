-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Types
import Control.Lens.Const
import Data.Contravariant
import Control.Lens.First
import Control.Monad.Identity
import Data.Bifunctor
import Data.Profunctor
import Data.Tagged

%default total

public export
Optic : (Type -> Type -> Type) -> (Type -> Type) -> Type -> Type -> Type -> Type -> Type
Optic p f s t a b = p a (f b) -> p s (f t)

public export
Simple : (Type -> Type -> Type -> Type -> Type) -> Type -> Type -> Type
Simple p s a = p s s a a

public export
Optic' : (Type -> Type -> Type) -> (Type -> Type) -> Type -> Type -> Type
Optic' p f = Simple (Optic p f)

public export
LensLike : (Type -> Type) -> Type -> Type -> Type -> Type -> Type
LensLike = Optic Morphism

public export
LensLike' : (Type -> Type) -> Type -> Type -> Type
LensLike' f = Simple (LensLike f)


-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
public export
Lens : Type -> Type -> Type -> Type -> Type
Lens s t a b  = { f : Type -> Type } -> Functor f => LensLike f s t a b

-- type Lens' s a = Lens s s a a
public export
Lens' : Type -> Type -> Type
Lens' = Simple Lens

-- type Getting r s a = (a -> Const r a) -> s -> Const r s
public export
Getting : Type -> Type -> Type -> Type
Getting r = LensLike' (Const r)

-- type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s
public export
Getter : Type -> Type -> Type
Getter s a = { f : Type -> Type } -> (Contravariant f, Functor f) => LensLike' f s a

-- type ASetter s t a b = (a -> Identity b) -> s -> Identity t
public export
Setter : Type -> Type -> Type -> Type -> Type
Setter = LensLike Identity

public export
Setter' : Type -> Type -> Type
Setter' = Simple Setter

-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
public export
Traversal : Type -> Type -> Type -> Type -> Type
Traversal s t a b  = { f : Type -> Type } -> Applicative f => LensLike f s t a b

-- type Traversal' s a = Traversal s s a a
public export
Traversal' : Type -> Type -> Type
Traversal' = Simple Traversal

-- type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
-- The choice of `Choice p` for a valid `Traversal` is the `(->)` implementation. Which is used
-- when using a prism as a getter or setter.

public export
Prism : Type -> Type -> Type -> Type -> Type
Prism s t a b  = {p : Type -> Type -> Type} -> { f : Type -> Type } ->
                 (Choice p,Applicative f) => Optic p f s t a b

public export
Prism' : Type -> Type -> Type
Prism' = Simple Prism

public export
Iso : Type -> Type -> Type -> Type -> Type
Iso s t a b = {p : Type -> Type -> Type} -> {f : Type -> Type} ->
              (Profunctor p,Functor f) => Optic p f s t a b

public export
Iso' : Type -> Type -> Type
Iso' = Simple Iso

-- type Review t b = forall p f. (Choice p, Bifunctor p, Settable f) => Optic' p f t b
public export
Review : Type -> Type -> Type -> Type -> Type
Review s t a b = { p : Type -> Type -> Type } -> { f : Type -> Type } ->
                 (Choice p, Bifunctor p) => Optic p Identity s t a b

-- type AReview t b = Optic' Tagged Identity t b
public export
AReview : Type -> Type -> Type
AReview t b = Optic' Tagged Identity t b



-- --------------------------------------------------------------------- [ EOF ]
