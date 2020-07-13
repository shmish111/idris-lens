module Data.Tagged

import Data.Bifunctor
import Data.Profunctor

%default total

public export
data Tagged : (a : Type) -> (b : Type) -> Type where
  MkTagged: b -> Tagged a b

public export
retag : Tagged s b -> Tagged t b
retag (MkTagged b) = MkTagged b

||| Alias for 'unTagged'
public export
untag : Tagged s b -> b
untag (MkTagged x) = x

||| Tag a value with its own type.
public export
tagSelf : a -> Tagged a a
tagSelf = MkTagged

||| 'asTaggedTypeOf' is a type-restricted version of 'const'. It is usually used as an infix operator, and its typing forces its first argument (which is usually overloaded) to have the same type as the tag of the second.
public export
asTaggedTypeOf : s -> Tagged s b -> s
asTaggedTypeOf = const

||| 'untagSelf' is a type-restricted version of 'untag'.
public export
untagSelf : Tagged a a -> a
untagSelf (MkTagged x) = x

public export
Semigroup a => Semigroup (Tagged s a) where
  (MkTagged a) <+> (MkTagged b) = MkTagged (a <+> b)

public export
Monoid a => Monoid (Tagged s a) where
  neutral = MkTagged neutral

public export
Functor (Tagged s) where
  map f (MkTagged x) = MkTagged (f x)

public export
implementation Applicative (Tagged s) where
  pure = MkTagged
  (MkTagged f) <*> (MkTagged x) = MkTagged (f x)

public export
implementation Monad (Tagged s) where
  (MkTagged m) >>= k = k m

public export
implementation Bifunctor Tagged where
  bimap _ g (MkTagged b) = MkTagged (g b)

public export
implementation Profunctor Tagged where
  dimap _ f (MkTagged s) = MkTagged (f s)
  lmap _ = retag
  rmap = map

public export
implementation Choice Tagged where
  left' (MkTagged b) = MkTagged (Left b)
  right' (MkTagged b) = MkTagged (Right b)
