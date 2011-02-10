{-# LANGUAGE GeneralizedNewtypeDeriving
           , RankNTypes
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , GADTs  
           #-}
                    
module ContextRefT where

import qualified Data.IntMap as IntMap
import Control.Monad.State hiding (mapM_, mapM, forM)

import Data.Foldable
import Data.Traversable 
import Data.Maybe

import Misc

import Prelude hiding (elem, and, concat, mapM, mapM_)

 -- this has no Eq, or Ord because they would basically never do what you expect
newtype ContextRef s = ContextRef { unRef :: Int }


data ContextData s t = ContextData Int (IntMap.IntMap Int) (IntMap.IntMap (t, [Int]))
  deriving (Functor, Foldable, Traversable)

newtype ContextT s t m a = ContextT { unContextT :: StateT (ContextData s t) m a}
 deriving (Monad, MonadFix)

  
-- #TODO, this is broken, we need to remove t from ContextT and move it to the references 
--runContextT :: (Monad m) => (forall s. ContextT s t m a) -> m a
runContextT m = evalStateT (unContextT m) (ContextData 0 IntMap.empty IntMap.empty)

newRef :: (Monad m) => t -> ContextT s t m (ContextRef s)
newRef a = ContextT $ do
  ContextData n x y <- get
  put $ ContextData (n + 1) (IntMap.insert n n x) (IntMap.insert n (a, [n]) y)
  return $ ContextRef n
  

readRef :: (Monad m) => ContextRef s -> ContextT s t m t
readRef (ContextRef ref) = ContextT $ do
  ContextData _ x y <- get
  let value = do
      valueIdx <- IntMap.lookup ref x
      liftM fst $ IntMap.lookup valueIdx y
  return $ case value of
      Just a -> a
      Nothing -> error "You tried to read a reference that hasn't been defined yet. \
                       \If you would've tried this in the IO or ST monad you would \
                       \have gotten an infinite loop. Rather than run around in \
                       \circles I'm going to politely die instead."
           
writeRef :: (Monad m) => ContextRef s -> t -> ContextT s t m ()
writeRef (ContextRef ref) a = ContextT $ do
  ContextData n x y <- get
  let maybeNewY = do
      valueIdx <- IntMap.lookup ref x
      (_, otherKeys) <- IntMap.lookup valueIdx y
      return $ IntMap.insert valueIdx (a, otherKeys) y
  case maybeNewY of 
    Just newY -> put $ ContextData n x newY
    Nothing -> error "You tried to write to a reference that hasn't been defined yet. \
                       \If you would've tried this in the IO or ST monad you would \
                       \have gotten an infinite loop. Rather than run around in \
                       \circles I'm going to politely die instead."
  
  
-- substitute a with b
subsRef :: (Monad m) => ContextRef s -> ContextRef s -> ContextT s t m ()
subsRef (ContextRef a) (ContextRef b) | a == b = return ()
subsRef (ContextRef a) (ContextRef b) = ContextT $ do
  ContextData n x y <- get
  let newData = do
      aIndex <- IntMap.lookup a x
      bIndex <- IntMap.lookup b x
      (_     , aKeys) <- IntMap.lookup aIndex y
      (bValue, bKeys) <- IntMap.lookup bIndex y
      let affectedRefs = aKeys ++ bKeys
      let refUpdates = IntMap.fromList $ zip affectedRefs $ repeat bIndex
      let newX = IntMap.union refUpdates x
      let newY = IntMap.insert bIndex (bValue, affectedRefs) y
      return (newX, newY, aIndex == bIndex)
  case newData of
    Just (newX, newY, eq) -> case eq of
        True -> return ()
        False -> put $ ContextData n newX newY -- (IntMap.delete aIndex) we don't delete because we may want to restore
    Nothing -> error "You tried to substitute a reference that hasn't been defined yet. \
                       \If you would've tried this in the IO or ST monad you would \
                       \have gotten an infinite loop. Rather than run around in \
                       \circles I'm going to politely die instead."
                       
subsRefs :: (Monad m) => [ContextRef s] -> ContextRef s -> ContextT s t m ()
subsRefs xs a = mapM_ (flip subsRef a) xs
  
refEq :: (Monad m) => ContextRef s -> ContextRef s -> ContextT s t m Bool
refEq (ContextRef a) (ContextRef b) | a == b = return True
refEq (ContextRef a) (ContextRef b) = ContextT $ do
  ContextData _ x _ <- get
  let maybeEqual = do
      aIndex <- IntMap.lookup a x
      bIndex <- IntMap.lookup b x
      return (aIndex == bIndex)
  case maybeEqual of
    Just r -> return r
    Nothing -> error "You tried to compare a reference that hasn't been defined yet. \
                       \If you would've tried this in the IO or ST monad you would \
                       \have gotten an infinite loop. Rather than run around in \
                       \circles I'm going to politely die instead."
                       
-- #TODO hmmm I'm going to have to redesign ContextT to make this safe :/  I'll do that later

-- This funciton is not perfect.  You could potentially pass in a reference that was created _after_ the context was forked.
-- Normally this would just cause an error, but it could potentially 'work' but not do what you want.
-- It would be nice to have a way to enforce this in the type system somehow
--forkContext :: (MonadFix m) => (forall s'. (ContextRef s -> ContextRef s') -> ContextT s' t m b) -> ContextT s t m b
forkContext f = ContextT $ do
  context <- get
  lift $ evalStateT (unContextT $ f caster) context
  where
    caster (ContextRef ref) = ContextRef ref

 
--forkMappedContext :: (MonadFix m) => (t -> t') -> (forall s'. (ContextRef s -> ContextRef s') -> ContextT s' t' m b) -> ContextT s t m b
forkMappedContext f g = ContextT $ do
  context <- get
  lift $ evalStateT (unContextT $ g caster) (fmap f context)
  where
    caster (ContextRef ref) = ContextRef ref

copySubGraph :: (MonadFix m, Traversable f, Functor f) => ContextRef s -> ContextT s (f (ContextRef s)) m (ContextRef s)
copySubGraph ref = do
  relevantNodes <- reachable ref
  lookupNew <- mfix $ \lookupNew -> do
    newNodes <- forM relevantNodes $ \x -> do
      newValue <- readRef x
      newRef =<< mapM lookupNew newValue
    let lookupNew' a = liftM fromJust $ lookupRef a $ zip relevantNodes newNodes
    return lookupNew'
  lookupNew ref
  
lookupRef :: (Monad m) => ContextRef s -> [(ContextRef s, a)] -> ContextT s t m (Maybe a)
lookupRef ref []          = return Nothing
lookupRef ref ((x,y):xys) = do
  yep <- refEq ref x
  case yep of
    True  -> return $ Just y
    False -> lookupRef ref xys

-- The returned list always includes the original reference
reachable :: (Foldable f, Monad m) => ContextRef s -> ContextT s (f (ContextRef s)) m [ContextRef s]
reachable ref = reachable' [] ref
reachable' :: (Monad m, Foldable f) => [ContextRef s] -> ContextRef s -> ContextT s (f (ContextRef s)) m [ContextRef s]
reachable' xs ref= do
  alreadyFound <- refElem ref xs
  case alreadyFound of
    True -> return []
    False -> do
      x <- readRef ref
      xs' <- liftM concat $ mapM (reachable' (ref:xs)) $ toList x
      return (ref:xs')

refElem :: (Monad m, Foldable f) => ContextRef s -> f (ContextRef s) -> ContextT s t m Bool
refElem ref t = refElem' $ toList t
  where
    refElem' []     = return False
    refElem' (x:xs) = do
      yep <- refEq ref x
      case yep of
        True  -> return True
        False -> refElem' xs
    

graphEq :: (Functor f, Eq (f ()), Foldable f, MonadFix m) 
        => ContextRef s -> ContextRef s -> ContextT s (f (ContextRef s)) m Bool
graphEq aRef' bRef' = forkContext $ \cast -> let 
    graphEq' a b = graphEq'' (cast a) (cast b)
    graphEq'' aRef bRef = do
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
  in graphEq' aRef' bRef'



