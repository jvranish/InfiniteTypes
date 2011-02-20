{-# LANGUAGE DoRec
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , StandaloneDeriving
           , FlexibleContexts
           , RankNTypes
           #-}
           
           
import Data.Char
import Data.Maybe
import Data.Foldable
import Data.Traversable

import Data.FixedList

import qualified Data.Map as Map
import qualified Data.List as List

import Control.Monad.Fix
import Control.Monad hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Reader hiding (mapM)

-- #TODO probably eventually remove this
import Control.Monad.Identity hiding (mapM)

--import qualified ParsecToken
import Text.Parsec hiding (char)
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import Fix
import Misc
import GraphT


import Prelude hiding (elem, mapM, foldl)

import Text.PrettyPrint.HughesPJClass

type TypeRef s = GraphRef s Type

data Type a = Var
            | Func a a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  
data Expr a = Apply a a
            | Id String
            | Lambda String a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  
-- I'm not very confident of how the precedence stuff is setup here.
instance (Pretty a) => Pretty (Type a) where
  pPrintPrec _ _ Var = text "Var"
  pPrintPrec l p (Func a b) = wrapParen p 1 $  pPrintPrec l 2 a <+> text "->" <+> pPrintPrec l 1 b

instance (Pretty a) => Pretty (Expr a) where
  pPrintPrec _ _ (Id name) = text name
  pPrintPrec l p (Lambda name a) = wrapParen p 0 $ char '\\' <> text name <+> text "->" <+> pPrintPrec l 0 a
  pPrintPrec l p (Apply a b) = wrapParen p 1 $  pPrintPrec l 1 a <+> pPrintPrec l 2 b
  
wrapParen :: (Ord a) => a -> a -> Doc -> Doc
wrapParen prio consPrio x | prio > consPrio = parens x
                          | otherwise       = x



  
unify :: (MonadFix m) => TypeRef s -> TypeRef s -> GraphT s Type m ()
unify aRef bRef = refEq aRef bRef ?? (return ()) $ bindM2 unify' (readRef aRef) (readRef bRef)
  where
    unify' Var _ = subsRef aRef bRef
    unify' _ Var = subsRef bRef aRef 
    unify' (Func a b) (Func c d) = do
                writeRef bRef $ Func a b
                subsRef aRef bRef
                unify a c
                unify b d
{-
It would be cooler and more flexible to convert the expr to a graph and then tie all the name bindings
  in the graph, and then run a SCC to determine the order to unify.
  but this isn't really helpful until I put in let bindings, so I'm going to use the stupid simple way
  
-}
  
infer :: (MonadFix m) => Y Expr -> ReaderT [(String, TypeRef s)] (GraphT s Type m) (TypeRef s)
infer (Y e) = case e of
  Apply a b -> do
    a' <- infer a
    b' <- infer b
    c <- lift $ newRef $ Var
    f <- lift $ newRef $ Func b' c
    lift $ unify a' f
    return c
  Lambda s a -> do
    x <- lift $ newRef $ Var
    a' <- local ((s, x):) $ infer a
    f <- lift $ newRef $ Func x a'
    return f
  Id s -> asks (fromJust . lookup s) -- will die on free variables, but that's ok with me for now


data EitherF f a = EitherF (Either String (f a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  
instance (Pretty (f a)) => Pretty (EitherF f a) where
  pPrintPrec _ _ (EitherF (Left s)) = text s
  pPrintPrec l p (EitherF (Right a)) = pPrintPrec l p a
  
deriving instance Foldable (Either e) -- neatest feature ever.
deriving instance Traversable (Either e)

showType :: (Monad m) => TypeRef s -> GraphT s Type m String
showType ref = do
  (a, t) <- flattenType ref
  return $ (prettyShow a)  ++ " : " ++ (List.intercalate ", " $ fmap showVarDef $ Map.assocs t)
  
showVarDef :: (Pretty a) => (String, a) -> String
showVarDef (s, a) = s ++ " = " ++ prettyShow a


--flattenType :: (Monad m, Traversable f) => ContextRef t -> ContextT s (f (ContextRef s)) m (Y (EitherF f), Map.Map String (Y (EitherF f)))
flattenType :: (Monad m) => GraphRef s Type -> GraphT s Type m (Y (EitherF Type), Map.Map String (Y (EitherF Type)))
flattenType ref = forkMappedContext (ref :. Nil) (EitherF . Right) $ \(ref' :. Nil) ->
    evalStateT (runStateT (flatten ref') Map.empty) varNames
    

flatten :: (Monad m) => GraphRef s (EitherF Type) -> StateT (Map.Map [Char] (Y (EitherF Type))) (StateT [String] (GraphT s (EitherF Type) m)) (Y (EitherF Type))
flatten ref = do
    hasCycle <- lift $ lift $ reachableFrom ref ref
    a <- lift $ lift $ readRef ref
    case a of
      EitherF (Right Var) -> liftM Y $ writeName ref =<< lift genSym
      _ -> case hasCycle of 
          True -> do {- replace node with new symbol, add symbol to map, fill out that row in the map -}
            newSym <- liftM (fmap toUpper) $ lift genSym
            x <- writeName ref newSym
            flatA <- mapM flatten a
            modifyT $ Map.insert newSym $ Y flatA
            return $ Y x
          False -> liftM Y $ mapM flatten a
  where
    writeName ref' varName = do
      let x = EitherF $ Left varName
      lift $ lift $ writeRef ref' x
      return x
  
-- sig should not be reachable from 'a' and viceversa (because that would be silly)
checkAgainstSig :: (MonadFix m) => TypeRef s -> TypeRef s -> GraphT s Type m Bool
checkAgainstSig sig a = forkContext (sig :. a :. Nil) $ \(sig' :. a' :. Nil) -> do
  sig'' <- copySubGraph $ sig'
  unify sig'' a'
  graphEq sig' sig''
  
  
lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser haskellStyle
identifier :: ParsecT String u Identity String
identifier = Token.identifier lexer
symbol :: String -> ParsecT String u Identity String
symbol = Token.symbol lexer
parseParens :: ParsecT String u Identity a -> ParsecT String u Identity a
parseParens = Token.parens lexer

parseExpr :: Parser (Y Expr)
parseExpr = parseApply <|> parseLambda <|> parseId 

parseApply :: Parser (Y Expr)
parseApply = parseParens $ do
  a <- parseExpr
  others <- many1 parseExpr
  return $ foldl (Y ... Apply) a others

parseId :: Parser (Y Expr)
parseId = do
  name <- identifier
  return $ Y $ Id name

parseLambda :: Parser (Y Expr)
parseLambda = do 
  _ <- symbol "\\"
  param <- identifier
  _ <- symbol "->"
  expr <- parseExpr
  return $ Y $ Lambda param expr
  

parseE :: String -> Y Expr
parseE s = case parse parseExpr "" s of
  Right a -> a
  Left e -> error (show e)

subsEnv :: Y Expr -> Y Expr
subsEnv a = runReader (transformFixM subsId a) env
  
i, k, c, b, s, y :: Y Expr
i = parseE "\\x -> x"                                      -- λx.x
k = parseE "\\x -> \\y -> x"                               -- λx.λy.x
c = parseE "\\x -> \\y -> \\z -> (x z y)"                  -- λx.λy.λz.(x z y)
b = parseE "\\x -> \\y -> \\z -> (x (y z))"                -- λx.λy.λz.(x (y z))
s = parseE "\\x -> \\y -> \\z -> (x z (y z))"              -- λx.λy.λz.(x z (y z))
y = parseE "\\f -> (\\x -> (f (x x)) \\x -> (f (x x)))"    -- λg.(λx.g (x x)) (λx.g (x x))

subsId :: Expr (Y Expr) -> Reader [(String, Y Expr)] (Y Expr)
subsId (Id s) = asks (fromJust . lookup s) -- will just die on free variable, but I'm ok with that for now
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
  a <- runReaderT (infer testA) []
  b <- runReaderT (infer testB) []
  ab <- checkAgainstSig a b
  ba <- checkAgainstSig b a
  return (ab, ba)

