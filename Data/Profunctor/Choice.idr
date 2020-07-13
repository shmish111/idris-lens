module Data.Profunctor.Choice

import Data.Profunctor.Class
import Data.Morphisms

public export
interface Profunctor p => Choice (p : Type -> Type -> Type) where
  left' : p a b -> p (Either a c) (Either b c)
  left' = dimap (either Right Left) (either Right Left) . right'

  right' : p a b -> p (Either c a) (Either c b)
  right' = dimap (either Right Left) (either Right Left) . left'

public export
Choice Morphism where
  left' (Mor f) = Mor g
    where g : Either a c -> Either b c
          g (Left a) = Left (f a)
          g (Right c) = Right c

  right' (Mor f) = Mor g
    where g : Either c a -> Either c b
          g (Left c) = Left c
          g (Right a) = Right (f a)
