
module Main where

import Control.Monad.Reader
import Control.Monad.Identity

import Data.Maybe

import Fix
import GraphT
import InfiniteTypes



subsEnv :: Y Expr -> Y Expr
subsEnv a = runReader (transformFixM subsId a) env
  
i, k, c, b, s, y :: Y Expr
i = parseE "\\x -> x"                                      -- ?x.x
k = parseE "\\x -> \\y -> x"                               -- ?x.?y.x
c = parseE "\\x -> \\y -> \\z -> (x z y)"                  -- ?x.?y.?z.(x z y)
b = parseE "\\x -> \\y -> \\z -> (x (y z))"                -- ?x.?y.?z.(x (y z))
s = parseE "\\x -> \\y -> \\z -> (x z (y z))"              -- ?x.?y.?z.(x z (y z))
y = parseE "\\f -> (\\x -> (f (x x)) \\x -> (f (x x)))"    -- ?g.(?x.g (x x)) (?x.g (x x))

subsId :: Expr (Y Expr) -> Reader [(String, Y Expr)] (Y Expr)
subsId (Id name) = asks (fromJust . lookup name) -- will just die on free variable, but I'm ok with that for now
subsId x = return $ Y x


env :: [(String, Y Expr)]
env = [ ("i", i)
      , ("k", k)
      , ("c", c)
      , ("b", b)
      , ("s", s)
      , ("y", y)
      ]

{-
Expr> y (b (c i) (c (b b (b c (c i)))))
(fix b . (a -> b -> (a -> c -> d) -> d) -> c) -> c)
Expr> y (b (c i) (b (c (b b (b c (c i)))) (b (c i) k)))
(fix c . ((a -> ((b -> c) -> d) -> (a -> d -> e) -> e) -> f) -> f)
-}

-- #TODO fix parser / pretty printer so that id = parser . prettyShow
testA, testB :: Y Expr
testA = subsEnv $ parseE "(y (b (c i) (c (b b (b c (c i))))))"
testB = subsEnv $ parseE "(y (b (c i) (b (c (b b (b c (c i)))) (b (c i) k))))"

--B : B = (a -> B -> (a -> c -> d) -> d) -> c) -> c
--C : C = ((a -> ((b -> C) -> d) -> (a -> d -> e) -> e) -> f) -> f
showInferType :: Y Expr -> String
showInferType x = runIdentity $ runGraphT $ showType =<< runReaderT (infer x) []
  
test :: (Bool, Bool)
test = runIdentity $ runGraphT $ do
  aType <- runReaderT (infer testA) []
  bType <- runReaderT (infer testB) []
  ab <- checkAgainstSig aType bType
  ba <- checkAgainstSig bType aType
  return (ab, ba)
  
main :: IO ()
main = do
  putStrLn $ showInferType testA
  putStrLn $ showInferType testB
  print test
  
