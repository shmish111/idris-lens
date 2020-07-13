-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Lens
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Tuple
import Control.Lens.Types
import Control.Lens.Lens
import Data.Profunctor

import Data.Bitraversable

%default total

--
-- Note: Tuples in Idris are nested `Pair`s, so these don't currently act like
-- their haskell counterparts, e.g., `_2` on `(1,2,3)` will focus on `(2,3)`.
--

public export
fst_ : Lens (a,c) (b,c) a b
fst_ = lens (\(a,_) => a)
          (\(_,c),b => (b,c))

public export
snd_ : Lens (c,a) (c,b) a b
snd_ = lens (\(_,a) => a)
          (\(c,_),b => (c,b))

public export
both : Bitraversable r => Traversal (r a a) (r b b) a b
both (Mor f) = Mor (bitraverse f f)

-- --------------------------------------------------------------------- [ EOF ]
