{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}
module ContextRefT where

import qualified Data.IntMap as IntMap
import Control.Monad.State hiding (mapM_)

import Data.Foldable
import Data.Maybe

import Misc

import Prelude hiding (elem, and, concat, mapM, mapM_)

newtype ContextRef s = ContextRef { unRef :: Int }
    deriving (Eq, Ord)


data ContextData s a = ContextData Int (IntMap.IntMap Int) (IntMap.IntMap (a, [Int]))

newtype ContextT s m t a = ContextT { unContextT :: StateT (ContextData s t) m a}
 deriving (Monad, MonadFix)


 
runContextT :: (Monad m) => (forall s. ContextT s m t a) -> m a
runContextT m = evalStateT (unContextT m) (ContextData 0 IntMap.empty IntMap.empty)

newRef :: (Monad m) => a -> ContextT s m a (ContextRef s)
newRef a = ContextT $ do
  ContextData n x y <- get
  put $ ContextData (n + 1) (IntMap.insert n n x) (IntMap.insert n (a, [n]) y)
  return $ ContextRef n
  

readRef :: (Monad m) => ContextRef s -> ContextT s m a a
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
           
writeRef :: (Monad m) => ContextRef s -> a -> ContextT s m a ()
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
subsRef :: (Monad m) => ContextRef s -> ContextRef s -> ContextT s m a ()
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
                       
subsRefs :: (Monad m) => [ContextRef s] -> ContextRef s -> ContextT s m a ()
subsRefs xs a = mapM_ (flip subsRef a) xs
  
refEq :: (Monad m) => ContextRef s -> ContextRef s -> ContextT s m a Bool
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
-- This funciton is not perfect.  You could potentially pass in a reference that was created _after_ the context was forked.
-- Normally this would just cause an error, but it could potentially 'work' but not do what you want.
-- This would be a nice thing to try in my experimental type system
forkContext :: (MonadFix m) => (forall s'. (ContextRef s -> ContextRef s') -> ContextT s' m a b) -> ContextT s m a b
forkContext f = ContextT $ do
  context@(ContextData n x y) <- get
  a <- lift $ evalStateT (unContextT $ f caster) context
  put $ (ContextData (n + 1) x y)
  return a
  where
    caster (ContextRef ref) = ContextRef ref
  

copySubGraph :: (MonadFix m, Foldable f, Functor f) => ContextRef s -> ContextT s m (f (ContextRef s)) (ContextRef s)
copySubGraph ref = do
  relevantNodes <- reachable ref
  lookupNew <- mfix $ \lookupNew -> do
    newNodes <- forM relevantNodes $ \x -> do
      newValue <- readRef x
      newRef $ fmap lookupNew newValue
    let lookupNew' a = fromJust $ lookup a $ zip relevantNodes newNodes
    return lookupNew'
  return $ lookupNew ref


reachable :: (Foldable f, Monad m) => ContextRef s -> ContextT s m (f (ContextRef s)) [ContextRef s]
reachable ref = reachable' [] ref
reachable' :: (Monad m, Foldable f) => [ContextRef s] -> ContextRef s -> ContextT s m (f (ContextRef s)) [ContextRef s]
reachable' xs ref | ref `elem` xs = return []
reachable' xs ref=  do
  x <- readRef ref
  xs' <- liftM concat $ mapM (reachable' (ref:xs)) $ toList x
  return (ref:xs')

graphEq :: (Functor f, Eq (f ()), Foldable f, MonadFix m) 
        => ContextRef s -> ContextRef s -> ContextT s m (f (ContextRef s)) Bool
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



