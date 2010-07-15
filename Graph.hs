{-# LANGUAGE GeneralizedNewtypeDeriving
           , RankNTypes
           #-}
-- #TODO: do proper module exports
module Graph where

import Control.Monad.Fix
import Control.Monad.Trans

import ContextRefT
import qualified Data.Map as Map

import Data.Maybe

data GraphContext s a = GraphContext (Map.Map (OuterNodeRef s a) (ContextRef s a)) (Map.Map (ContextRef s a) a)

newtype GraphT s b m a = GraphT { unGraphT :: (ContextT s (ContextRef s b) (ContextT s b m) a) }
  deriving (Monad, MonadFix, Functor)

instance MonadTrans (GraphT s b) where
  lift k = GraphT $ lift $ lift k
  
type OuterNodeRef s a = ContextRef s (ContextRef s a)

newtype GraphRef s a = GraphRef (OuterNodeRef s a)
  deriving (Eq, Ord)
  

runGraphT :: (Monad m) => (forall s. GraphT s b m a) -> m a
runGraphT m = runContextT $ unsafeRunContextT $ unGraphT m

unsafeRunGraphT :: (Monad m) => GraphT s b m a -> m a
unsafeRunGraphT m = unsafeRunContextT $ unsafeRunContextT $ unGraphT m

addNode :: (Monad m) => a -> GraphT s a m (GraphRef s a)
addNode x = GraphT $ do
  ref <- lift $ newRef x
  outerRef <- newRef ref
  return $ GraphRef outerRef
  
readNode :: (Monad m) => GraphRef s a -> GraphT s a m a
readNode (GraphRef outerRef) = GraphT $ readRef outerRef >>= lift . readRef

updateNode :: (Monad m) => GraphRef s a -> a -> GraphT s a m ()
updateNode (GraphRef outerRef) x = GraphT $ do
  ref <- readRef outerRef
  lift $ writeRef ref x

subsNode :: (Monad m) => GraphRef s a -> GraphRef s a -> GraphT s a m ()
subsNode (GraphRef oldOuterRef) (GraphRef newOuterRef) = GraphT $ do
  ref <- readRef newOuterRef
  oldRef <- readRef oldOuterRef
  outer <- saveContext
  let refsToUpdate = Map.keys $ Map.filter (== oldRef) outer
  mapM_ (flip writeRef ref) refsToUpdate

save :: (Monad m) => GraphT s b m (GraphContext s b)
save = GraphT $ do
  outer <- saveContext
  inner <- lift $ saveContext
  return $ GraphContext outer inner
  
restore :: (Monad m) => GraphContext s b -> GraphT s b m ()
restore (GraphContext outer inner) = GraphT $ do
  restoreContext outer
  lift $ restoreContext inner
  
allNodes :: (Monad m) => GraphT s b m (Map.Map (GraphRef s b) b)  
allNodes = do
  GraphContext outer inner <- save
  return $ Map.mapKeys GraphRef $ mergeMap outer inner

mergeMap :: (Ord k, Ord a) => Map.Map k a -> Map.Map a b -> Map.Map k b
mergeMap a b = Map.fromList $ catMaybes $ fmap lookupValue $ Map.keys a
  where
    lookupValue k = Map.lookup k a >>= flip Map.lookup b >>= return . ((,) k)

