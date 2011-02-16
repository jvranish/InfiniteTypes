{-# LANGUAGE TypeFamilies
           #-}

module MonadicEq where

import Data.Foldable

import Control.Monad
import ContextRefT

import Prelude hiding (elem, lookup)

class EqM a where
  type EqMonad a :: * -> *
  (==:), (/=:) :: a -> a -> EqMonad a Bool
  -- a /=: b = liftM not $ (a ==: b)
  -- a ==: b = liftM not $ (a /=: b)  (Monad (EqMonad a)) => 

infix  4  ==:, /=:


instance EqM (ContextRef s t) where
  type EqMonad (ContextRef s t) = ContextT s t m
  a ==: b = refEq a b
  

--instance EqM (ContextT s t m) where
--  type Val (ContextT s t m) = ContextRef s t

--instance EqM (ContextT s t m) where
--  type Val (ContextT s m) = ContextRef s t  
--lookup

--elem


--lookup :: (Monad m) => ContextRef s t-> [(ContextRef s t, a)] -> ContextT s t m (Maybe a)
lookup a []          = return Nothing
lookup a ((x,y):xys) = do
  yep <- a ==: x
  case yep of
    True  -> return $ Just y
    False -> lookup a xys

--elem :: (Monad m, Foldable f) => ContextRef s -> f (ContextRef s) -> ContextT s t m Bool
elem a t = elem' $ toList t
  where
    elem' []     = return False
    elem' (x:xs) = do
      yep <- a ==: x
      case yep of
        True  -> return True
        False -> elem' xs

