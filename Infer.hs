{-# LANGUAGE GeneralizedNewtypeDeriving
           , RankNTypes
           #-}
           
{- #TODO 

figure out a way to chain derefs politely

fix inference bug
  bind for uses
  bind for definitions
make nice type error messages
  group errors in the let bindings
make cool control structure for pointers
add some more built in functions
add control structures for (if, for, foreach, while, do, case)
more complete use of structs
user definable types
algebraic datatypes
  pattern matching
  
-}
module Infer where

import Fix
import Graph as Graph

import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)

import Data.Maybe
import Data.Foldable hiding (elem, concat)
import Data.Traversable

import qualified Data.Map as Map

import Prelude hiding (sequence, mapM, mapM_, foldr)

newtype TypeRef s f = TypeRef (Graph.GraphRef s (TypeNode s f))
  deriving (Eq, Ord)

type TypeNode s f = f (TypeRef s f)

-- #TODO remove b
newtype InferT s f b m a = InferT { unInferT :: Graph.GraphT s (TypeNode s f) m a }
  deriving (Monad, MonadFix, Functor)
  
instance MonadTrans (InferT s f b) where
  lift k = InferT $ lift k
  
runInferT :: (Monad m) => (forall s. InferT s f b m a) -> m a
runInferT m = Graph.unsafeRunGraphT $ unInferT m

newType :: (Monad m) => TypeNode s f -> InferT s f b m (TypeRef s f)
newType x = InferT $ liftM TypeRef $  Graph.addNode x

readType :: (Monad m) => TypeRef s f -> InferT s f b m (TypeNode s f)
readType (TypeRef ref) = InferT $  Graph.readNode ref

subsType :: (Monad m) => TypeRef s f -> TypeRef s f -> InferT s f b m ()
subsType (TypeRef refA) (TypeRef refB) = InferT $  Graph.subsNode refA refB

updateType :: (Monad m) => TypeRef s f -> TypeNode s f -> InferT s f b m ()
updateType (TypeRef ref) x = InferT $  Graph.updateNode ref x

save :: (Monad m) => InferT s f b m (Graph.GraphContext s (TypeNode s f))
save = InferT $ Graph.save

restore :: (Monad m) => Graph.GraphContext s (TypeNode s f) -> InferT s f b m ()
restore oldContext = InferT $  Graph.restore oldContext

allTypes :: (Monad m) => InferT s f b m (Map.Map (TypeRef s f) (TypeNode s f))
allTypes = InferT $ do
  x <- Graph.allNodes
  return $ Map.mapKeys TypeRef x

copyType :: (MonadFix m, Foldable f, Functor f) => TypeRef s f -> InferT s f b m (TypeRef s f)
copyType ref = do
  relevantNodes <- reachable ref
  lookupNew <- mfix $ \lookupNew -> do
    newNodes <- forM relevantNodes $ \x -> do
      newValue <- readType x
      --newType $ fmap lookupNew newValue
      newType $ fmap lookupNew newValue
    let lookupNew' a = fromJust $ lookup a $ zip relevantNodes newNodes
    return lookupNew'
  return $ lookupNew ref


reachable :: (Foldable f, Monad m) => TypeRef s f -> InferT s f b m [TypeRef s f]
reachable ref = reachable' [] ref
reachable' :: (Monad m, Foldable f) => [TypeRef s f] -> TypeRef s f -> InferT s f b m [TypeRef s f]
reachable' xs ref | ref `elem` xs = return []
reachable' xs ref=  do
  x <- readType ref
  xs' <- liftM concat $ mapM (reachable' (ref:xs)) $ toList x
  return (ref:xs')

treeify :: (Monad m, Traversable f) => TypeRef s f -> InferT s f b m (Y f)
treeify ref = treeify' [] ref
treeify' :: (Monad m, Traversable f) => [TypeRef s f] -> TypeRef s f -> InferT s f b m (Y f)
treeify' xs ref | ref `elem` xs = error "infered infinite type, something is broken"
treeify' xs ref = readType ref >>= liftM Y . (mapM (treeify' (ref:xs)))


replaceTypes :: (Monad m) => TypeRef s f -> TypeRef s f -> TypeRef s f -> InferT s f b m (TypeRef s f)
replaceTypes a b n = subsType a n >> subsType b n >> return n

type Key = Int

subGraph :: (Monad m, Functor f, Foldable f) =>  TypeRef s f -> InferT s f b m [(f Key, Key, [Key])]
subGraph ref = do
  xs <- reachable ref
  let keyMap = Map.fromList $ zip xs [0..]
  let newKey oldKey = keyMap Map.! oldKey
  let graphNode a = do
      x <- readType a
      let node = fmap newKey x
      return (node, newKey a, toList node)
  mapM graphNode xs


