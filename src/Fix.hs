{-# LANGUAGE FlexibleContexts
           , UndecidableInstances #-}
module Fix where

import Data.Traversable

import Text.PrettyPrint.HughesPJClass

import Prelude hiding (mapM)

data Y f = Y { unY :: (f (Y f)) }

instance (Eq (f (Y f))) => Eq (Y f) where
  (Y a) == (Y b) = a == b
  
instance (Ord (f (Y f))) => Ord (Y f) where
  compare (Y a) (Y b) = compare a b
  
instance (Show (f (Y f))) => Show (Y f) where
  -- #TODO make another nicer show like instance
  show (Y a) = "(" ++ show a ++ ")"-- show "Y " ++ show a
  
instance (Pretty (f (Y f))) => Pretty (Y f) where
  pPrintPrec l p (Y a) = pPrintPrec l p a
  
--  transformFix applies the monadic transform function f, to a structure
--  from the bottom up
transformFixM :: ( Traversable f, Monad m) => (f b -> m b) -> Y f -> m b
transformFixM f (Y x) = f =<< mapM (transformFixM f) x

transformFix :: (Functor f) => (f b -> b) -> Y f -> b
transformFix f (Y x) = f $ fmap (transformFix f) x

fixify :: (Functor f) => f (f (f (f (f (f (f (f (f (f (f (Y f))))))))))) -> Y f
fixify t = Y $ fmap (Y . fmap (Y . fmap (Y . fmap (Y . fmap (Y . fmap (Y . fmap (Y . fmap (Y . fmap (Y . fmap Y))))))))) t

{-
class FixM m where
  type FixM m
  unFixM :: FixM m -> f
  
instance FixM (GraphT s f n)
  type FixM (GraphT s f m) = GraphRef s f
  unFixM :: Y f -> m (f (Y f))
  mkFixM :: f (Y f) -> m (Y f)
  
-}