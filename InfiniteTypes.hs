{-# LANGUAGE RecursiveDo
           #-}
import Data.Tree
import Data.Graph
import Infer

import Data.Foldable
import Data.Traversable

import Data.Monoid

import Control.Applicative
import Control.Monad.Trans

squishGraph g = drawForest $ fmap (fmap show) $ dff g'
  where
    (g', vn, kv) = graphFromEdges g


data Test a = Two Int a a
            | Zero Int
  deriving (Eq, Ord, Show)
  
instance Functor Test where
  fmap f (Two n a b) = Two n (f a) (f b)
  fmap _ (Zero n) = Zero n
  
instance Foldable Test where
  foldMap f (Two _ a b) = f a `mappend` f b
  foldMap _ (Zero _) = mempty
  
instance Traversable Test where
  traverse f (Two n a b) = Two n <$> f a <*> f b
  traverse _ (Zero n) = pure $ Zero n
  
test :: IO ()
test = runInferT $ mdo
  a <- newType $ Two 5 a b
  b <- newType $ Zero 3
  c <- newType $ Two 5 e e
  d <- newType $ Two 2 c e
  e <- newType $ Zero 3
  g <- subGraph c
  lift $ putStrLn $ squishGraph g
  
