{-# LANGUAGE GeneralizedNewtypeDeriving
           , RankNTypes
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , DoRec
           , GADTs
           , TypeFamilies
           , MultiParamTypeClasses
           #-}
           -- #TODO clean these up 
                    
module ContextT where

import qualified Data.IntMap as IntMap
import Control.Monad.State hiding (mapM_, mapM, forM)

import Data.Foldable
import Data.Traversable 
import Data.Maybe

import Data.IORef
import Debug.Trace
import System.IO.Unsafe

import Misc

import Fix -- #TODO remove this

import Prelude hiding (elem, and, concat, mapM, mapM_)

 -- this has no Eq, or Ord because they would basically never do what you expect
data ContextRef s a = ContextRef Int a
-- the value that is stored in the reference is only the initial value

data ContextData t = ContextData Int (IntMap.IntMap Int) (IntMap.IntMap (t, [Int]))
  deriving (Functor, Foldable, Traversable)

newtype ContextT s t m a = ContextT { unContextT :: StateT (ContextData t) m a}
 deriving (Monad, MonadFix, MonadTrans) -- #TODO derive other classes too

  
-- #TODO, this is broken, we need to remove t from ContextT and move it to the references 
runContextT :: (Monad m) => (forall s. ContextT s t m a) -> m a
runContextT m = unsafeRunContextT m

unsafeRunContextT m = evalStateT (unContextT m) (ContextData 0 IntMap.empty IntMap.empty)

{-# NOINLINE counter #-}
counter = unsafePerformIO $ newIORef (0 :: Int)

nextRef a = unsafePerformIO $ do
  x <- readIORef counter
  writeIORef counter (x+1)
  return (trace (show x) x)

newRef :: (Monad m) => t -> ContextT s t m (ContextRef s t)
newRef a = ContextT $ do
  --ContextData n x y <- get
  --put $ ContextData (n + 1) x y -- (IntMap.insert n n x) (IntMap.insert n (a, [n]) y)
  --return $ ContextRef n a
  return $ ContextRef (nextRef a) a
  
  

readRef :: (Monad m) => ContextRef s t -> ContextT s t m t
readRef (ContextRef ref a) = ContextT $ do
  ContextData _ x y <- get
  let value = do -- we don't call lookupRef here because we don't need to commit the ref here if it's not in the map
      valueIdx <- IntMap.lookup ref x
      liftM fst $ IntMap.lookup valueIdx y
  return $ case value of
      Just val -> val
      Nothing -> a
         -- #TODO think about this some more, and see if we should have an error condition here or not
         --   This sometimes might not do what people expect
         --      error "You tried to read a reference that hasn't been defined yet. \
         --            \If you would've tried this in the IO or ST monad you would \
         --            \have gotten an infinite loop. Rather than run around in \
         --            \circles I'm going to politely die instead."
           
writeRef :: (Monad m) => ContextRef s t -> t -> ContextT s t m ()
writeRef ref a = do
  ContextData n x y <- ContextT $ get
  (valueIdx, _, otherKeys) <- lookupRef ref
  ContextT $ put $ ContextData n x (IntMap.insert valueIdx (a, otherKeys) y)


-- #TODO | do not expose
lookupRef (ContextRef ref a) = ContextT $ do
  ContextData n x y <- get
  let result = do
      index <- IntMap.lookup ref x
      (value, keys) <- IntMap.lookup index y
      return (index, value, keys)
  case result of
    Just val -> return val
    Nothing -> do
      put $ ContextData n (IntMap.insert ref ref x) (IntMap.insert ref (a, [ref]) y)
      return (ref, a, [ref])
  
-- #TODO probably remove this
commitRef ref = lookupRef ref >> return ()

-- substitute a with b
subsRef :: (Monad m) => ContextRef s t -> ContextRef s t -> ContextT s t m ()
subsRef (ContextRef a _) (ContextRef b _) | a == b = return ()
subsRef aRef bRef = do
  ContextData n x y <- ContextT $ get
  (aIndex, _     , aKeys) <- lookupRef aRef
  (bIndex, bValue, bKeys) <- lookupRef bRef
  let affectedRefs = aKeys ++ bKeys
  let refUpdates = IntMap.fromList $ zip affectedRefs $ repeat bIndex
  let newX = IntMap.union refUpdates x
  let newY = IntMap.insert bIndex (bValue, affectedRefs) y
  case aIndex == bIndex of
    True -> return ()
    False -> ContextT $ put $ ContextData n newX newY
                       
subsRefs :: (Monad m) => [ContextRef s t] -> ContextRef s t -> ContextT s t m ()
subsRefs xs a = mapM_ (flip subsRef a) xs
  
refEq :: (Monad m) => ContextRef s t -> ContextRef s t -> ContextT s t m Bool
refEq (ContextRef a _) (ContextRef b _) | a == b = return True
refEq (ContextRef a _) (ContextRef b _) = ContextT $ do
  ContextData _ x _ <- get
  let maybeEqual = do
      aIndex <- IntMap.lookup a x
      bIndex <- IntMap.lookup b x
      return (aIndex == bIndex)
  case maybeEqual of
    Just r -> return r
    Nothing -> return False -- if either one is not commited yet, and the ref Id's themselfs are not equal, then their not equal


-- This funciton is not perfect.  You could potentially pass in a reference that was created _after_ the context was forked.
-- Normally this would just cause an error, but it could potentially 'work' but not do what you want.
-- It would be nice to have a way to enforce this in the type system somehow
{-
forkContext :: (MonadFix m) => [ContextRef s t] -> ((forall s'. [ContextRef s' t'] -> ContextT s' t' m b) -> ContextT s t m b
forkContext refs f = do
  mapM_ lookupRef refs -- force ref commit
  context <- ContextT $ get
  ContextT $ lift $ evalStateT (unContextT $ f $ fmap caster refs) context
  where
    caster (ContextRef ref a) = ContextRef ref a
-}
--test :: MonadFix m => ContextT s (Y (ContextRef s)) m String   -- (ContextRef s (Y (ContextRef s))) -- String --

--test :: MonadFix m => ContextT s (Y (ContextRef s)) m String
{-
test = do
  rec
    a <- newRef $ Y a
  forkContext [a] $ \[x] -> do
    v <- readRef x
    r <- newRef $ Y x
    return "asdf"
    --return x
  --b <- newRef $ Just a
-}

{-
data GWitness b a where
  GWitness :: (b ~ a) => GWitness b a
  
data Witness a where
  Witness :: Witness Int

data HideType f where
  HideType :: (Witnesses f a) => a -> f a -> HideType f
  
  
class Witnesses f a where
  mkWitness :: f a
  
--data MyPhantom a = MyPhantom
  
class DoesWitness a where
  data MyWitness a :: *
  mk :: MyWitness a
  
instance DoesWitness Int where
  data MyWitness a | a == Int = MyPhantom
  mk = MyPhantom

instance Witnesses Witness Int where
  mkWitness = Witness
  
mytest :: HideType Witness
mytest = HideType (5::Int) mkWitness

data HideType2 where
  HideType2 :: (DoesWitness a) => a -> MyWitness a -> HideType2

mytest2 :: HideType2 
mytest2 = HideType2 (5::Int) mk

realTest (HideType a MyPhantom) = a

-}

  {-

  


asdf :: HideType Witness -> Int
asdf (HideType a Witness) = a
--asdf _ = undefined



mytest :: HideType Witness
mytest = HideType (5::Int) (mkWitness (5::Int))
  -}
--class ExtractType

{-
-- #TODO hmmm I'm going to have to redesign ContextT to make this safe :/  I'll do that later


 
--forkMappedContext :: (MonadFix m) => (t -> t') -> (forall s'. (ContextRef s -> ContextRef s') -> ContextT s' t' m b) -> ContextT s t m b
forkMappedContext f g = ContextT $ do
  context <- get
  lift $ evalStateT (unContextT $ g caster) (fmap f context)
  where
    caster (ContextRef ref a) = ContextRef ref a

copySubGraph :: (MonadFix m, Traversable f, Functor f) => ContextRef s a -> ContextT s (f (ContextRef s a)) m (ContextRef s a)
copySubGraph ref = do
  relevantNodes <- reachable ref
  lookupNew <- mfix $ \lookupNew -> do
    newNodes <- forM relevantNodes $ \x -> do
      newValue <- readRef x
      newRef =<< mapM lookupNew newValue
    let lookupNew' a = liftM fromJust $ lookupRef a $ zip relevantNodes newNodes
    return lookupNew'
  lookupNew ref
  
lookupRef :: (Monad m) => ContextRef s t-> [(ContextRef s t, a)] -> ContextT s t m (Maybe a)
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

-}

