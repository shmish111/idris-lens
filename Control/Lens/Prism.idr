-- -------------------------------------------------------------- [ Lens.idr ]
-- Description : Idris port of Control.Applicative
-- Copyright   : (c) Huw Campbell
-- --------------------------------------------------------------------- [ EOH ]
module Control.Lens.Prism
import Control.Lens.Types
import Control.Monad.Identity
import Data.Profunctor

%default total

||| Create a `Prism`
public export
prism : (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (map bt)) . right'

public export
prism' : (b -> s) -> (s -> Maybe a) -> Prism s s a b
prism' bs sma = prism bs (\s => maybe (Left s) Right (sma s))

-- Note that in Haskell the naming convention is _Left however _ isn't allowed in Idris2
public export
left_ : Prism (Either a c) (Either b c) a b
left_ = prism Left $ either Right (Left . Right)

public export
right_ : Prism (Either c a) (Either c b) a b
right_ = prism Right $ either (Left . Left) Right

public export
just_ : Prism (Maybe a) (Maybe b) a b
just_ = prism Just $ maybe (Left Nothing) Right

public export
nothing_ : Prism' (Maybe a) ()
nothing_ = prism' (const Nothing) $ maybe (Just ()) (const Nothing)

-- --------------------------------------------------------------------- [ EOF ]
