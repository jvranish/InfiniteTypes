{-# LANGUAGE RecursiveDo
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           #-}
import Infer

import Data.Foldable
import Data.Traversable

import Data.Monoid

import Control.Applicative
import Control.Monad.Trans


type TypeRef s = ContextRef s


data Type a = Var
            | Func a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Expr a = Apply a a
            | Id String
            | Lambda String a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  
unify :: TypeRef s -> TypeRef s -> ContextT s m a (TypeRef s)
unify aRef bRef = do
    sameRef <- refEq aRef bRef
    case sameRef of
      True -> return a
      False -> mdo
          a <- readRef aRef
          b <- readRef bRef
          subsRefs [a, b] n  -- the awesome happens here
          n <- unify' a b  
          return n
  where
    unify' Var _ = bRef
    unify' _ Var = aRef
    unify' (Func a b) (Func c d) = newType =<< return Func `ap` unify a c `ap` unify b d
    

  
  
