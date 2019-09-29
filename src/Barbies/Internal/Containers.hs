{-# LANGUAGE UndecidableInstances #-}
module Barbies.Internal.Containers
  (
    Container(..)
  , ErrorContainer(..)
  )

where

import Data.Functor.Barbie
import Data.Bifunctor (first)
import Data.Bitraversable (bitraverse)
import Data.Functor.Const
import GHC.Generics (Generic)


-- {{ Container ---------------------------------------------------------------

-- | Wrapper for barbies that act as containers of @a@
--   by wearing @('Const' a)@.
newtype Container b a
  = Container { getContainer :: b (Const a) }
  deriving  (Generic)

deriving instance Eq  (b (Const a)) => Eq  (Container b a)
deriving instance Ord (b (Const a)) => Ord (Container b a)

deriving instance Read (b (Const a)) => Read (Container b a)
deriving instance Show (b (Const a)) => Show (Container b a)

instance FunctorB b => Functor (Container b) where
  fmap f
    = Container . (bmap (first f)) . getContainer

instance TraversableB b => Foldable (Container b) where
  foldMap f
    = bfoldMap (f . getConst) . getContainer

instance TraversableB b => Traversable (Container b) where
    traverse f
      = fmap Container . btraverse (bitraverse f pure) . getContainer

instance ApplicativeB b => Applicative (Container b) where
    pure a
      = Container $ bpure (Const a)

    l <*> r
      = Container $ bzipWith appConst (getContainer l) (getContainer r)
      where
        appConst :: Const (a -> a') x -> Const a x -> Const a' x
        appConst (Const f) (Const a)
          = Const (f a)

-- }} Container ---------------------------------------------------------------


-- {{ ErrorContainer ----------------------------------------------------------

-- | Wrapper for barbies that act as containers of @e@
--   by wearing @'Either' e@.
newtype ErrorContainer b e
  = ErrorContainer { getErrorContainer :: b (Either e) }
  deriving (Generic)


deriving instance Eq  (b (Either  e)) => Eq  (ErrorContainer b e)
deriving instance Ord (b (Either  e)) => Ord (ErrorContainer b e)

deriving instance Read (b (Either  e)) => Read (ErrorContainer b e)
deriving instance Show (b (Either  e)) => Show (ErrorContainer b e)


instance FunctorB b => Functor (ErrorContainer b) where
  fmap f
    = ErrorContainer . (bmap (first f)) . getErrorContainer

instance TraversableB b => Foldable (ErrorContainer b) where
  foldMap f
    = bfoldMap (either f (const mempty)) . getErrorContainer

instance TraversableB b => Traversable (ErrorContainer b) where
    traverse f
      = fmap ErrorContainer . btraverse (bitraverse f pure) . getErrorContainer

-- }} ErrorContainer ----------------------------------------------------------
