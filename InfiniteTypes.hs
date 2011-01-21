{-# LANGUAGE DoRec
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , StandaloneDeriving
           , FlexibleContexts
           , RankNTypes
           #-}
           
           
-- #TODO remove Eq instance for refs
-- add check signature function
-- fix thingy type name, do I want to still keep Y around?


import Data.Char
import Data.Maybe
import Data.Foldable
import Data.Traversable

import qualified Data.Map as Map
import qualified Data.List as List

--import Data.Monoid

--import Control.Applicative
import Control.Monad.Fix
import Control.Monad hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Reader hiding (mapM)

import Fix
import Misc
import ContextRefT

import Prelude hiding (elem, mapM)

type TypeRef s = ContextRef s


data Type a = Var
            | Func a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  
-- #TODO use pprint instead, cause show is silly
{-
instance (Show a) => Show (Mythingy Type a) where
  show (Var) = "@"
  show (Func a@(Func _ _) b) = "(" ++ show a ++ ")" ++ " -> " ++ show b
  show (Func a b) = show a ++ " -> " ++ show b
-}

data Expr a = Apply a a
            | Id String
            | Lambda String a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  
unify :: (MonadFix m) => TypeRef s -> TypeRef s -> ContextT s (Type (TypeRef s)) m (TypeRef s)
unify aRef bRef = do
    sameRef <- refEq aRef bRef
    case sameRef of
      True -> return aRef
      False -> do
          a <- readRef aRef
          b <- readRef bRef
          rec 
            subsRefs [aRef, bRef] n  -- the awesome happens here
            n <- unify' a b  
          return n
  where
    unify' Var _ = return bRef
    unify' _ Var = return aRef
    unify' (Func a b) (Func c d) = newRef =<< return Func `ap` unify a c `ap` unify b d
{-
It would be cooler and more flexible to convert the expr to a graph and then tie all the name bindings
  in the graph, and then run a SCC to determine the order to unify.
  but this isn't really helpful until I put in let bindings, so I'm going to use the stupid simple way
  
-}
  
infer :: (MonadFix m) => Y Expr -> ReaderT [(String, TypeRef s)] (ContextT s (Type (TypeRef s)) m) (TypeRef s)
infer (Y e) = infer' e
  where
    infer' (Apply a b) = do
      a' <- infer a
      b' <- infer b
      c <- lift $ newRef $ Var
      f <- lift $ newRef $ Func b' c
      _ <- lift $ unify a' f
      return c
    infer' (Id s) = asks (fromJust . lookup s) -- will die on free variables, but that's ok with me for now
    infer' (Lambda s a) = do
      x <- lift $ newRef $ Var
      a' <- local ((s, x):) $ infer a
      f <- lift $ newRef $ Func x a'
      return f

{-
print out type,
parse type from signature
-}

--data Thing a = Thing a (Map String a)
--convert all items to Right items
{-
  check if reachable
-}
--Either String (f a)

-- 
-- Check to see if a is reachable from b. Will return false if a == b unless there is a cycle
-- # TODO remove elem !!!!FIX THIS!!! and remove the EQ instance
reachableFrom :: (Foldable f, Monad m) => ContextRef s -> ContextRef s -> ContextT s (f (ContextRef s)) m Bool
reachableFrom a b = liftM (elem a) $ concatMapM reachable =<< return . toList =<< readRef b

data Mythingy f a = Mythingy (Either String (f a))
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  
instance (Show (f a)) => Show (Mythingy f a) where
  show (Mythingy (Left s)) = s
  show (Mythingy (Right a)) = show a
  
deriving instance Foldable (Either e) -- neatest feature EVAR!
deriving instance Traversable (Either e)


showType ref = do
  (a, t) <- flattenType ref
  return $ List.intercalate ", " $ fmap showVarDef $ Map.assocs t
  
showVarDef (s, a) = s ++ " = " ++ show a


--flattenType :: (Monad m, Traversable f) => ContextRef t -> ContextT s (f (ContextRef s)) m (Y (Mythingy f), Map.Map String (Y (Mythingy f)))
flattenType ref = forkMappedContext (Mythingy . Right) $ \cast ->
    evalStateT (runStateT (flatten (cast ref)) Map.empty) varNames
    
--flatten :: (Monad m, Traversable f) => ContextRef s -> StateT (Map.Map String (Y (Mythingy f))) (StateT [String] (ContextT s (Mythingy f (ContextRef s)) m)) (Y (Mythingy f))
flatten ref = do
  hasCycle <- lift $ lift $ reachableFrom ref ref
  a <- lift $ lift $ readRef ref
  case a of
    Mythingy (Right Var) -> do
      varName <- lift $ genSym
      return $ Y $ Mythingy $ Left varName
    _ -> case hasCycle of 
        True -> do {- replace node with new symbol, add symbol to map, fill out that row in the map -}
          newSym <- liftM (fmap toUpper) $ lift $ genSym
          let x = Mythingy $ Left newSym
          lift $ lift $ writeRef ref x
          flatA <- mapM flatten a
          modifyT $ Map.insert newSym $ Y flatA
          return $ Y x
        False -> liftM Y $ mapM flatten a
      {-
  case a of
    Mythingy (Right x) -> 
    Mythingy (Left x) -> return $ Mythingy $ Left x -}
  
-- #TODO make a check against signature function

checkAgainstSig sig a = forkContext $ \cast -> do
  sig' <- copySubGraph $ cast sig
  ab <- unify sig' (cast a)
  graphEq sig ab
  
