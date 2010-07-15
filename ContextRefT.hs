{-# LANGUAGE GeneralizedNewtypeDeriving
           , RankNTypes
           #-}
module ContextRefT
         ( ContextRef
         , ContextT
         , runContextT
         , unsafeRunContextT
         , newRef
         , readRef
         , writeRef
         , refKey
         , saveContext
         , restoreContext
         ) where

--import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Control.Monad.State

newtype ContextRef s a = ContextRef Int
    deriving (Eq, Ord)

data ContextData s a = ContextData Int (Map.Map (ContextRef s a) a)

newtype ContextT s b m a = ContextT { unContextT :: StateT (ContextData s b) m a}
 deriving (Monad, MonadFix, MonadTrans, Functor)

runContextT :: (Monad m) => (forall s. ContextT s b m a) -> m a
runContextT m = evalStateT (unContextT m) (ContextData 0 Map.empty)

unsafeRunContextT :: (Monad m) => ContextT s b m a -> m a
unsafeRunContextT m = evalStateT (unContextT m) (ContextData 0 Map.empty)

newRef :: (Monad m) => a -> ContextT s a m (ContextRef s a)
newRef a = ContextT $ do
  ContextData n t <- get
  let ref = ContextRef n
  put $ ContextData (n + 1) (Map.insert ref a t)
  return $ ref
  

readRef :: (Monad m) => ContextRef s a -> ContextT s a m a 
readRef ref = ContextT $ do
  ContextData _ t <- get
  return $ case Map.lookup ref t of 
    Just x -> x
    Nothing -> error "error doing readref"
  
  --return $ (Map.!) t ref -- should be safe since we are never allowed to delete a reference 
                            -- and the existentials prevent references from "escaping"
           
writeRef :: (Monad m) => ContextRef s a -> a -> ContextT s a m ()
writeRef ref a = ContextT $ do
  ContextData n t <- get
  put $ ContextData n (Map.insert ref a t)
       
refKey :: ContextRef s a -> Int  
refKey (ContextRef ref) = ref


-- Any references created after a save, but before a restore will still be accessible after the restore
--saveContext :: (Monad m) => ContextT s b m (ContextT s b m ())
saveContext :: (Monad m) => ContextT s b m (Map.Map (ContextRef s b) b)
saveContext = ContextT $ do
  ContextData _ old <- get
  return old

-- We don't want to save off the whole structure here cause then we might invalidate some references
--  on the restore.
-- We just want to restore the _values_ of references
-- Any new references made after the save, but before the restore, will still be valid
restoreContext :: (Monad m) => Map.Map (ContextRef s b) b -> ContextT s b m ()
restoreContext old = ContextT $ do
  ContextData n current <- get
  put $ ContextData n (Map.union old current)
  
{-
ContextT $ do
    ContextData _ old <- get
    -- We don't want to save off the whole structure here cause then we might invalidate some references
    --  on the restore.
    -- We just want to restore the _values_ of references
    let restore = ContextT $ do
        ContextData n current <- get
        put $ ContextData n (IntMap.union old current)
    return restore
  -}

