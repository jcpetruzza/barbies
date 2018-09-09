-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Barbie.Container
--
-- We get a container of @a@'s for any Barbie-type when we make it wear a
-- @('Const' a)@ . The 'Container' wrapper gives us the expected
-- instances for a container type.
----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module Data.Barbie.Container
  (
    Container(..)
  )

where

import Data.Barbie
import Data.Bifunctor (first)
import Data.Bitraversable (bitraverse)
import Data.Coerce (coerce)
import Data.Functor.Const
import Data.Functor.Prod (uncurryn)
import GHC.Generics (Generic)

-- | Wrapper for container-Barbies.
newtype Container b a =
  Container { getContainer :: b (Const a) }
  deriving  (Generic)

deriving instance Eq  (b (Const a)) => Eq  (Container b a)
deriving instance Ord (b (Const a)) => Ord (Container b a)

deriving instance Read (b (Const a)) => Read (Container b a)
deriving instance Show (b (Const a)) => Show (Container b a)

instance FunctorB b => Functor (Container b) where
  fmap f =
    Container . (bmap (first f)) . getContainer

instance TraversableB b => Foldable (Container b) where
  foldMap f =
    getConst . btraverse (coerce . first f) . getContainer

instance TraversableB b => Traversable (Container b) where
    traverse f =
      fmap Container . btraverse (bitraverse f pure) . getContainer

instance ProductB b => Applicative (Container b) where
    pure a
      = Container $ buniq (Const a)

    l <*> r
      = Container $ bmap (uncurryn appConst) (getContainer l /*/ getContainer r)
      where
        appConst :: Const (a -> a') x -> Const a x -> Const a' x
        appConst (Const f) (Const a)
          = Const (f a)


