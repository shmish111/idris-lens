-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.At

import Control.Lens.Setter
import Control.Lens.Types
import Control.Monad.Identity
import Data.Profunctor
import Data.List

%default total

public export
interface Ixed ( m : Type ) where
  IxInd : Type
  IxVal : Type
  ix : IxInd -> { f : Type -> Type } -> Applicative f => LensLike' f m IxVal

public export
implementation {a : Type} -> Ixed (List a) where
  IxInd = Nat
  IxVal = a
  ix k (Mor h) = Mor (\xs0 => go xs0 k)
    where
      go : List a -> Nat -> f (List a)
      go Nil _           = pure Nil
      go (a :: as) Z     = (:: as) <$> (h a)
      go (a :: as) (S n) = (a ::)  <$> (go as n)

public export
implementation {a : Type} -> Ixed (Maybe a) where
  IxInd = Unit
  IxVal = a
  ix _ (Mor f) = Mor (\g => case g of
                  (Just a) => Just <$> f a
                  Nothing  => pure Nothing
                  )

public export
interface At m where
  AtInd : Type
  AtVal : Type
  -- |
  -- >>> Map.fromList [(1,"world")] ^.at 1
  -- Just "world"
  --
  -- >>> at 1 ?~ "hello" $ Map.empty
  -- fromList [(1,"hello")]
  --
  -- /Note:/ 'Map'-like containers form a reasonable instance, but not 'Array'-like ones, where
  -- you cannot satisfy the 'Lens' laws.
  at : AtInd -> { f : Type -> Type } -> Applicative f => LensLike' f m (Maybe AtVal)

public export
sans : At m => AtInd { m } -> m -> m
sans k m = m |> at k .~ Nothing

public export
implementation {a : Type} -> At (Maybe a) where
  AtInd = Unit
  AtVal = a
  at () (Mor f) = (Mor f)

-- --------------------------------------------------------------------- [ EOF ]
