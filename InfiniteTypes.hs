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

-- #TODO probably eventually remove this
import Control.Monad.Identity hiding (mapM)
import Data.IORef

import Fix
import Misc
import GraphT

import Prelude hiding (elem, mapM)

type TypeRef s = GraphRef s Type


data Type a = Var
            | Func a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  
data Expr a = Apply a a
            | Id String
            | Lambda String a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  
unify :: (MonadFix m) => TypeRef s -> TypeRef s -> GraphT s Type m (TypeRef s)
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
    unify' (Func a b) (Func c d) = do
      rec
        r <- newRef $ Func a' b'
        a' <- unify a c
        b' <- unify b d
      return r
    -- newRef =<< return Func `ap` unify a c `ap` unify b d
{-
It would be cooler and more flexible to convert the expr to a graph and then tie all the name bindings
  in the graph, and then run a SCC to determine the order to unify.
  but this isn't really helpful until I put in let bindings, so I'm going to use the stupid simple way
  
-}
  
infer :: (MonadFix m) => Y Expr -> ReaderT [(String, TypeRef s)] (GraphT s Type m) (TypeRef s)
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
#TODO
print out type,
parse type from signature
-}



-- #TODO find a better name for this thingy
data Mythingy f a = Mythingy (Either String (f a))
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  
instance (Show (f a)) => Show (Mythingy f a) where
  show (Mythingy (Left s)) = s
  show (Mythingy (Right a)) = show a
  
deriving instance Foldable (Either e) -- neatest feature ever.
deriving instance Traversable (Either e)

showType :: (Monad m) => TypeRef s -> GraphT s Type m String
showType ref = do
  (a, t) <- flattenType ref
  return $ List.intercalate ", " $ fmap showVarDef $ Map.assocs t
  
showVarDef (s, a) = s ++ " = " ++ show a


--flattenType :: (Monad m, Traversable f) => ContextRef t -> ContextT s (f (ContextRef s)) m (Y (Mythingy f), Map.Map String (Y (Mythingy f)))
flattenType ref = forkMappedContext [ref] (Mythingy . Right) $ \[ref'] ->
    evalStateT (runStateT (flatten ref') Map.empty) varNames
    
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
  
-- sig should not be reachable from 'a' and viceversa (because that would be silly)
checkAgainstSig :: (MonadFix m) => TypeRef s -> TypeRef s -> GraphT s Type m Bool
checkAgainstSig sig a = forkContext [sig, a] $ \[sig', a'] -> do
  sig'' <- copySubGraph $ sig'
  ab <- unify sig'' a'
  graphEq sig' ab
  
y = fixify $ Lambda "f" (Apply (Lambda "x" (Apply (Id "f") (Apply (Id "x") (Id "x")))) (Lambda "x" (Apply (Id "f") (Apply (Id "x") (Id "x")))))
--y = fixify $ Lambda "x"  (Apply (Id "x") (Id "x"))
--(Lambda "x" (Apply (Id "f") (Apply (Id "x") (Id "x"))))
--b = Lambda "x" (Lambda "y" (Lambda "z" (Apply (Id "x") (Apply (Id "y") (Id "z"))))
--c
--i
--k
{-
Expr> y (b (c i) (c (b b (b c (c i)))))
(fix b . (a -> b -> (a -> c -> d) -> d) -> c) -> c)
Expr> y (b (c i) (b (c (b b (b c (c i)))) (b (c i) k)))
(fix c . ((a -> ((b -> c) -> d) -> (a -> d -> e) -> e) -> f) -> f)
-}

--B = (a -> B -> (a -> c -> d) -> d) -> c) -> c
--C = ((a -> ((b -> C) -> d) -> (a -> d -> e) -> e) -> f) -> f
--Y = λf.(λx.f (x x)) (λx.f (x x))
test = runIdentity $ runGraphT $ do
  yType <- runReaderT (infer y) []
  showType yType
  --return "Asdf"
  

--test :: MonadFix m => ContextT s (Y (ContextRef s)) m String
-- test2 :: MonadFix m => GraphT s Maybe m String
test2 = runIdentity $ runGraphT $  do
  rec
    a <- newRef $ Just b
    writeRef a $  Just b
    b <- newRef $ Just a
  forkContext [b] $ \[x] -> do
    v <- readRef x
    r <- newRef $ Just x
    return "asdf"
    --return x
  --b <- newRef $ Just a
  
  
{-
The magic features will be:
substitute, that does not look into values (and causes reference substitute)
newRef must not affect the state, (substituting a newref that is created after the substitute needs to work)
we could perhaps hold the value in the reference until an update happens

-}
  
testIO = do
  rec
    a <- newIORef (Y b)
    b <- newIORef (Y a)
  return a
  
