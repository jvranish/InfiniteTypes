{-# LANGUAGE FlexibleContexts
           , UndecidableInstances #-}
module Fix where

import Data.Traversable

import Prelude hiding (mapM)

data Y f = Y { unY :: (f (Y f)) }

instance (Eq (f (Y f))) => Eq (Y f) where
  (Y a) == (Y b) = a == b
  
instance (Ord (f (Y f))) => Ord (Y f) where
  compare (Y a) (Y b) = compare a b
  
instance (Show (f (Y f))) => Show (Y f) where
  -- #TODO make another nicer show like instance
  show (Y a) = show a -- show "Y " ++ show a
  
--  transformFix applies the monadic transform function f, to a structure
--  from the bottom up
transformFixM :: ( Traversable t, Monad m) => (t b -> m b) -> Y t -> m b
transformFixM f (Y x) = f =<< mapM (transformFixM f) x

transformFix :: (Functor f) => (f b -> b) -> Y f -> b
transformFix f (Y x) = f $ fmap (transformFix f) x
