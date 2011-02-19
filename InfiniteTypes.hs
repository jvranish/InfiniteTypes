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

import Data.FixedList

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.IntMap as IntMap

--import Data.Monoid

--import Control.Applicative
import Control.Monad.Fix
import Control.Monad hiding (mapM)
import Control.Monad.State hiding (mapM)
import Control.Monad.Reader hiding (mapM)

-- #TODO probably eventually remove this
import Control.Monad.Identity hiding (mapM)

--import qualified ParsecToken
import Text.Parsec hiding (char)
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import Fix
import Misc
import GraphT


import Prelude hiding (elem, mapM, foldl)

--import Text.PrettyPrint
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
  
wrapParen prio consPrio x | prio > consPrio = parens x
                          | otherwise       = x

lexer = Token.makeTokenParser haskellStyle
identifier = Token.identifier lexer
symbol = Token.symbol lexer
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
  symbol "\\"
  param <- identifier
  symbol "->"
  expr <- parseExpr
  return $ Y $ Lambda param expr

  
unify :: (MonadFix m) => TypeRef s -> TypeRef s -> GraphT s Type m (TypeRef s)
unify aRef bRef = do
    sameRef <- refEq aRef bRef
    case sameRef of
      True -> return aRef
      False -> do
          a <- readRef aRef
          b <- readRef bRef
          case (a, b) of
            (Var,   _) -> subsRef aRef bRef
            (_  , Var) -> subsRef bRef aRef
            (Func a b, Func c d) -> do
              rec
                writeRef bRef $ Func a' b'
                --n <- newRef $ Func a' b'   -- this should work, why not?
                --subsRef bRef n
                --writeRef bRef $ Func a b
                subsRef aRef bRef
                a' <- unify a c
                b' <- unify b d
              return ()
          return aRef
    return aRef
          
{-
          --rec
          --  subsRefs [aRef, bRef] n  -- the awesome happens here
          --  n <- unify' a b
          n <- unify' a b    
          return aRef
  where
    unify' Var _ = subsRef aRef bRef -- >> return bRef
    unify' _ Var = subsRef bRef aRef -- >> return aRef
    unify' (Func a b) (Func c d) = do
      rec
        writeRef bRef $ Func a' b'
        subsRef aRef bRef
        
        a' <- unify a c
        b' <- unify b d
      return ()
      -}
    {-
    unify' Var _ = return bRef
    unify' _ Var = return aRef
    unify' (Func a b) (Func c d) = newRef =<< return Func `ap` unify a c `ap` unify b d
    do
      rec
        r <- newRef $ Func a' b'
        a' <- unify a c
        b' <- unify b d
      return r
      -}
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
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)
  
instance ( Pretty (f a)) => Pretty (Mythingy f a) where
  pPrintPrec _ _ (Mythingy (Left s)) = text s
  pPrintPrec l p (Mythingy (Right a)) = pPrintPrec l p a
  
deriving instance Foldable (Either e) -- neatest feature ever.
deriving instance Traversable (Either e)

showType :: (Monad m) => TypeRef s -> GraphT s Type m String
showType ref = do
  (a, t) <- flattenType ref
  return $ (prettyShow a)  ++ " : " ++ (List.intercalate ", " $ fmap showVarDef $ Map.assocs t)
  
showVarDef (s, a) = s ++ " = " ++ prettyShow a


--flattenType :: (Monad m, Traversable f) => ContextRef t -> ContextT s (f (ContextRef s)) m (Y (Mythingy f), Map.Map String (Y (Mythingy f)))
flattenType ref = forkMappedContext (ref :. Nil) (Mythingy . Right) $ \(ref' :. Nil) ->
    evalStateT (runStateT (flatten ref') Map.empty) varNames
    
--flatten :: (Monad m, Traversable f) => ContextRef s -> StateT (Map.Map String (Y (Mythingy f))) (StateT [String] (ContextT s (Mythingy f (ContextRef s)) m)) (Y (Mythingy f))
flatten ref = do
  hasCycle <- lift $ lift $ reachableFrom ref ref
  a <- lift $ lift $ readRef ref
  case a of
    Mythingy (Right Var) -> do
      varName <- lift $ genSym
      let x = Mythingy $ Left varName
      lift $ lift $ writeRef ref x
      return $ Y x
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
checkAgainstSig sig a = forkContext (sig :. a :. Nil) $ \(sig' :. a' :. Nil) -> do
  sig'' <- copySubGraph $ sig'
  ab <- unify sig'' a'
  graphEq sig' ab
  
--y = fixify $ Lambda "f" (Apply (Lambda "x" (Apply (Id "f") (Apply (Id "x") (Id "x")))) (Lambda "x" (Apply (Id "f") (Apply (Id "x") (Id "x")))))

--y = fixify $ Lambda "x"  (Apply (Id "x") (Id "x"))
--(Lambda "x" (Apply (Id "f") (Apply (Id "x") (Id "x"))))
--b = Lambda "x" (Lambda "y" (Lambda "z" (Apply (Id "x") (Apply (Id "y") (Id "z"))))
--c
--i
--k
parseE s = case parse parseExpr "" s of
  Right a -> a
  Left e -> error (show e)

subsEnv a = runReader (transformFixM subsEnvM a) env
  
myC x y z = x z y
myB x y z = x (y z)
myK x y = x

i :: Y Expr
i = parseE "\\x -> x"                                      -- λx.x
k = parseE "\\x -> \\y -> x"                               -- λx.λy.x
c = parseE "\\x -> \\y -> \\z -> (x z y)"                  -- λx.λy.λz.(x z y)
b = parseE "\\x -> \\y -> \\z -> (x (y z))"                -- λx.λy.λz.(x (y z))
s = parseE "\\x -> \\y -> \\z -> (x z (y z))"              -- λx.λy.λz.(x z (y z))
y = parseE "\\f -> (\\x -> (f (x x)) \\x -> (f (x x)))"    -- λg.(λx.g (x x)) (λx.g (x x))
-- #TODO fix parser / pretty printer so that id = parser . prettyShow
testA = subsEnv $ parseE "(y (b (c i) (c (b b (b c (c i))))))"
testB = subsEnv $ parseE "(y (b (c i) (b (c (b b (b c (c i)))) (b (c i) k))))"
testC = subsEnv $ parseE "(b (c i) (c (b b (b c (c i)))))"
testD = subsEnv $ parseE "(b c (c i))"
testE = subsEnv $ parseE "(b c)" -- (c i))"
testF = subsEnv $ parseE "c" -- (c i))"
-- (myB (myC id) (myC (myB myB (myB myC (myC id)))))

subsEnvM :: Expr (Y Expr) -> Reader [(String, Y Expr)] (Y Expr)
subsEnvM (Id s) = asks (fromJust . lookup s) -- will just die on free variable, but I'm ok with that for now
subsEnvM x = return $ Y x



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


--B : B = (a -> B -> (a -> c -> d) -> d) -> c) -> c
--C : C = ((a -> ((b -> C) -> d) -> (a -> d -> e) -> e) -> f) -> f
--Y = λf.(λx.f (x x)) (λx.f (x x))
test x = runIdentity $ runGraphT $ do
  --typeEnv <- runReaderT (mapM (\(s, e) -> liftM ((,) s) (infer e)) env) []
  result <- runReaderT (infer x) []
  -- x <- readRef result
  --(ContextT.ContextData n x y )<- dumpStuff
  showType result
  -- return (show (refId result) ++ show x ++ show y ++ show result) --(fmap (snd . snd) $ IntMap.toList y))
  --return "Asdf"
  
test1 = runIdentity $ runGraphT $ do
  a <- runReaderT (infer testA) []
  sig <- copySubGraph a
  b <- runReaderT (infer testB) []
  --c <- graphEq a b 
  c <- unify a b
  --return c
  --showType c
  ab <- checkAgainstSig sig c
  --ba <- checkAgainstSig b a
  --return (ab, ba)
  return ab

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

