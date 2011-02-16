{-# LANGUAGE GeneralizedNewtypeDeriving
           , RankNTypes
           , DoRec
           #-}
           -- #TODO clean these up 
{-

, DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , DoRec
           , GADTs
           , TypeFamilies
           , MultiParamTypeClasses
           
           -}
 
module GraphT where

-- #TODO perhaps add non transformer runContext
import Data.Maybe
import Data.Foldable
import Data.Traversable

import Misc

import qualified ContextT as ContextT

import Control.Monad.State hiding (mapM_, mapM, forM)

import Prelude hiding (elem, and, concat, mapM, mapM_)


newtype GraphT s f m a = GraphT { unGraphT :: ContextT.ContextT s (f (GraphRef s f)) m a }
 deriving (Monad, MonadFix, MonadTrans) -- #TODO derive other classes too

data GraphRef s f = GraphRef { unGraphRef :: ContextT.ContextRef s (f (GraphRef s f)) }

--runContextT :: (Monad m) => (forall s. ContextT s t m a) -> m a
--runGraphT m = evalStateT (unContextT $ unGraphT m) (ContextData 0 IntMap.empty IntMap.empty)
runGraphT :: (Monad m) => (forall s. GraphT s f m a) -> m a
runGraphT m = ContextT.unsafeRunContextT $ unGraphT m

newRef :: (Monad m) => f (GraphRef s f) -> GraphT s f m (GraphRef s f)
newRef a = liftM GraphRef $ GraphT $ ContextT.newRef a

readRef :: (Monad m) => GraphRef s f -> GraphT s f m (f (GraphRef s f))
readRef (GraphRef ref) = GraphT $ ContextT.readRef ref

writeRef :: (Monad m) => GraphRef s f -> f (GraphRef s f) -> GraphT s f m ()
writeRef (GraphRef ref) a = GraphT $ ContextT.writeRef ref a

subsRef :: (Monad m) => GraphRef s f -> GraphRef s f -> GraphT s f m ()
subsRef (GraphRef a) (GraphRef b) = GraphT $ ContextT.subsRef a b

subsRefs :: (Monad m) => [GraphRef s f] -> GraphRef s f -> GraphT s f m ()
subsRefs xs (GraphRef ref) = GraphT $ ContextT.subsRefs (fmap unGraphRef xs) ref

refEq :: (Monad m) => GraphRef s f -> GraphRef s f -> GraphT s f m Bool
refEq (GraphRef a) (GraphRef b) = GraphT $ ContextT.refEq a b

-- #TODO | do not expose
--castRef :: (Functor f) => GraphRef s f -> GraphRef s' f
castRef :: (Functor f) => (f (GraphRef s' g) -> g (GraphRef s' g)) -> GraphRef s f -> GraphRef s' g
castRef f (GraphRef (ContextT.ContextRef ref a)) = GraphRef $ ContextT.ContextRef ref (f (fmap (castRef f) a))


--forkContext :: (Monad m, Functor f) => [GraphRef s f] -> (forall s'. [GraphRef s' f] -> GraphT s' f m b) -> GraphT s f m b
--forkContext refs f = do
  --GraphT $ mapM_ ContextT.lookupRef (fmap unGraphRef refs) -- force ref commit
  --context <- GraphT $ ContextT.ContextT $ get
  --GraphT $ ContextT.ContextT $ lift $ evalStateT (ContextT.unContextT $ unGraphT $ f $ fmap (castRef id) refs) context

forkContext :: (Monad m, Functor f) => [GraphRef s f] -> (forall s'. [GraphRef s' f] -> GraphT s' f m b) -> GraphT s f m b  
forkContext refs f = forkMappedContext refs id f
  
  

-- #TODO should I make this more general? (the type is unnecessarily constricting
-- perhaps use a fixed width type instead of a list  (actually yes, I should do this, use Vector or something)
forkMappedContext :: (Monad m, Functor f) => [GraphRef s f] -> (forall a. f a -> g a) -> (forall s'. [GraphRef s' g] -> GraphT s' g m b) -> GraphT s f m b
-- #TODO also check to see if the f needs to be polymorphic (I think it does)
-- forkMappedContext :: (Monad m, Functor f) => [GraphRef s f] -> (f (GraphRef s' g) -> g (GraphRef s' g)) -> (forall s'. [GraphRef s' g] -> GraphT s' g m b) -> GraphT s f m b
--forkMappedContext :: (Functor f, Monad m, Foldable t, Functor t) => t (GraphRef s f) -> (f (GraphRef s' g) -> g (GraphRef s' g)) -> (t (GraphRef s' g) -> GraphT s' g m a) -> GraphT s f m a
forkMappedContext refs f g = do
  GraphT $ mapM_ ContextT.lookupRef (fmap unGraphRef refs) -- force ref commit
  context <- GraphT $ ContextT.ContextT $  get
  GraphT $ ContextT.ContextT $ lift $ evalStateT (ContextT.unContextT $ unGraphT $ g $ fmap (castRef f) refs) (fmap (f . fmap (castRef f))  context)


  
-- #TODO decide on a naming convention for GraftT and ContextT  (Modules, Monads, and Refs. )
--   we probably just want to change the GraphT module name to GraphRefT, or the ContextT to ContextT (along with filenames)
    
refElem :: (Monad m, Foldable g) => GraphRef s f -> g (GraphRef s f) -> GraphT s f m Bool
refElem ref t = refElem' $ toList t
  where
    refElem' []     = return False
    refElem' (x:xs) = do
      yep <- refEq ref x
      case yep of
        True  -> return True
        False -> refElem' xs

lookupRef :: (Monad m) => GraphRef s f -> [(GraphRef s f, a)] -> GraphT s f m (Maybe a)
lookupRef ref []          = return Nothing
lookupRef ref ((x,y):xys) = do
  yep <- refEq ref x
  case yep of
    True  -> return $ Just y
    False -> lookupRef ref xys
    

copySubGraph :: (MonadFix m, Traversable f, Functor f) => GraphRef s f -> GraphT s f m (GraphRef s f)
copySubGraph ref = do
  relevantNodes <- reachable ref
  lookupNew <- mfix $ \lookupNew -> do
    newNodes <- forM relevantNodes $ \x -> do
      newValue <- readRef x
      newRef =<< mapM lookupNew newValue
    let lookupNew' a = liftM fromJust $ lookupRef a $ zip relevantNodes newNodes
    return lookupNew'
  lookupNew ref

-- 
-- Check to see if a is reachable from b. Will return false if a == b unless there is a cycle
reachableFrom :: (Foldable f, Monad m) => GraphRef s f -> GraphRef s f -> GraphT s f m Bool
reachableFrom a b = refElem a =<< concatMapM reachable =<< return . toList =<< readRef b
  
-- The returned list always includes the original reference
reachable :: (Foldable f, Monad m) => GraphRef s f -> GraphT s f m [GraphRef s f]
reachable ref = reachable' [] ref
reachable' :: (Monad m, Foldable f) => [GraphRef s f] -> GraphRef s f -> GraphT s f m [GraphRef s f]
reachable' xs ref= do
  alreadyFound <- refElem ref xs
  case alreadyFound of
    True -> return []
    False -> do
      x <- readRef ref
      xs' <- liftM concat $ mapM (reachable' (ref:xs)) $ toList x
      return (ref:xs')
      

graphEq :: (Functor f, Eq (f ()), Foldable f, Monad m) => GraphRef s f -> GraphRef s f -> GraphT s f m Bool
graphEq aRef' bRef' = forkContext [aRef', bRef'] $ \[a, b] -> let 
    graphEq' aRef bRef = do
      eq <- refEq aRef bRef
      case eq of
        True -> return True
        False -> do 
          a <- readRef aRef
          b <- readRef bRef
          case headEq a b of 
            False -> return False
            True -> do
              subsRef aRef bRef
              liftM and $ zipWithM graphEq' (toList a) (toList b)
  in graphEq' a b
  
  