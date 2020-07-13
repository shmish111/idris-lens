module Control.Lens.Review
import Data.Contravariant
import Control.Lens.Types
import Control.Lens.Getter
import Control.Monad.Identity
import Data.Profunctor
import Data.Tagged

%default total

infixr 8 #

public export
(#) : AReview t b -> b -> t
(#) p = runIdentity . untag . p . MkTagged . Id

public export
re : AReview t b -> Getter b t
re p = to (p #)
