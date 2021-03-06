module Control.Lens.Iso

import Control.Lens.Types
import Control.Monad.Identity
import Data.Profunctor

%default total

public export
data Exchange a b s t = MkExchange (s -> a) (b -> t)

public export
Functor (Exchange a b s) where
  map f (MkExchange sa bt) = MkExchange sa (f . bt)

public export
Profunctor (Exchange a b) where
  dimap f g (MkExchange sa bt) = MkExchange (sa . f) (g . bt)

  lmap f (MkExchange sa bt) = MkExchange (sa . f) bt

  rmap g (MkExchange sa bt) = MkExchange sa (g . bt)

public export
AnIso : Type -> Type -> Type -> Type -> Type
AnIso s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)

public export
AnIso' : Type -> Type -> Type
AnIso' = Simple AnIso

public export
iso : (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (map bt)

public export
withIso : AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k = case ai (MkExchange id Id) of
    MkExchange sa bt => k sa (runIdentity . bt)

public export
from : AnIso s t a b -> Iso b a t s
from l = withIso l $ \sa,bt => iso bt sa
