{-# LANGUAGE RecursiveDo
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           #-}

import Data.Maybe
import Data.Foldable
import Data.Traversable

--import Data.Monoid

--import Control.Applicative
import Control.Monad.Fix
import Control.Monad

import Control.Monad.Reader

import Fix
import ContextRefT


type TypeRef s = ContextRef s


data Type a = Var
            | Func a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Expr a = Apply a a
            | Id String
            | Lambda String a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  
unify :: (MonadFix m) => TypeRef s -> TypeRef s -> ContextT s m (Type (TypeRef s)) (TypeRef s)
unify aRef bRef = do
    sameRef <- refEq aRef bRef
    case sameRef of
      True -> return aRef
      False -> mdo
          a <- readRef aRef
          b <- readRef bRef
          subsRefs [aRef, bRef] n  -- the awesome happens here
          n <- unify' a b  
          return n
  where
    unify' Var _ = return bRef
    unify' _ Var = return aRef
    unify' (Func a b) (Func c d) = newRef =<< return Func `ap` unify a c `ap` unify b d
{-
It would be cooler and more flexible to convert the expr to a graph and then tie all the name bindings
  in the graph, and then run a SCC to determine the order unify.
  but this isn't really helpful until I put in let bindings, so I'm going to use the stupid simple way
  
-}
  
infer :: (MonadFix m) => Y Expr -> ReaderT [(String, TypeRef s)] (ContextT s m (Type (TypeRef s))) (TypeRef s)
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


