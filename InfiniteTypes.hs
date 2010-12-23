{-# LANGUAGE RecursiveDo
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           #-}
import ContextRefT

import Data.Foldable
import Data.Traversable

--import Data.Monoid

--import Control.Applicative
import Control.Monad.Fix
import Control.Monad


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
    

  
  
