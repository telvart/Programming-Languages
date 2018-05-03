{-# LANGUAGE GADTs #-}

import Text.ParserCombinators.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Calculator language extended with an environment to hold defined variables

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  deriving (Show,Eq)

-- Value defintion for statically scoped eval

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> TFBAE -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

-- Enviornment for statically scoped eval

type Env = [(String,FBAEVal)]

subst :: String -> FBAE -> FBAE -> FBAE
subst i v (Num x) = (Num x)
subst i v (Boolean x) = (Boolean x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (App l r) = (App (subst i v l) (subst i v r))
subst i v (Lambda i2 t b) = (Lambda i2 t (subst i v b))
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (Bind i' v2 b) =
        if(i == i')
        then (Bind i' (subst i v v2) b)
        else (Bind i' (subst i v v2) (subst i v b))
subst i v (Id x) = if(i==x) then v else (Id x)
subst i v (IsZero x) = IsZero (subst i v x)
subst i v (If c t e) = If (subst i v c) (subst i v t) (subst i v e)
subst i v (Fix a) = Fix (subst i v a)

-- Statically scoped eval

evalM :: Env -> FBAE -> (Maybe FBAEVal)
evalM env (Num n) = return (NumV n)

evalM env (Boolean b) = return (BooleanV b)

evalM env (Plus l r) = do { (NumV l') <- (evalM env l);
                            (NumV r') <- (evalM env r);
                            return (NumV (l'+r')) }

evalM env (Minus l r) =  do { (NumV l') <- (evalM env l);
                              (NumV r') <- (evalM env r);
                              if (l' > r')
                              then return (NumV (l' - r'))
                              else Nothing }

evalM env (Mult l r) =  do { (NumV l') <- (evalM env l);
                             (NumV r') <- (evalM env r);
                             return (NumV (l'*r')) }

evalM env (Div l r) =  do {  (NumV l') <- (evalM env l);
                             (NumV r') <- (evalM env r);
                             if (r' == 0)
                             then Nothing
                             else return (NumV (div l' r')) }

evalM env (And l r) =  do {  (BooleanV l') <- (evalM env l);
                             (BooleanV r') <- (evalM env r);
                             return (BooleanV (l' && r')) }

evalM env (Or l r) =  do {  (BooleanV l') <- (evalM env l);
                            (BooleanV r') <- (evalM env r);
                            return (BooleanV (l' || r')) }

evalM env (Leq l r) =  do {  (BooleanV l') <- (evalM env l);
                             (BooleanV r') <- (evalM env r);
                             return (BooleanV (l' <= r')) }

evalM env (IsZero expr) = do { (NumV n) <- (evalM env expr);
                               return (BooleanV (n == 0)) }

evalM env (If c t e) = do { (BooleanV b) <- (evalM env c);
                            if (b)
                            then (evalM env t)
                            else (evalM env e) }

evalM env (Bind i v b) = do { value <- (evalM env v);
                              (evalM ((i,value):env) b)}

evalM env (Lambda i t b) = return (ClosureV i t b env)

evalM env (App f arg) = do { value <- (evalM env arg);
                             (ClosureV i t b e) <- (evalM env f);
                             (evalM ((i,value):e) b ) }


evalM env (Id name) = (lookup name env)

evalM env (Fix func) = do { (ClosureV i t b e) <- (evalM env func);
                            evalM env (subst i (Fix (Lambda i t b)) b) }
-- Type inference function

type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM ctx (Num n) = return TNum;
typeofM ctx (Boolean b) = return TBool;

typeofM ctx (Plus l r)= do { tl <- typeofM ctx l;
                             tr <- typeofM ctx r;
                             if(tl == TNum && tr == TNum)
                             then return TNum
                             else Nothing }

typeofM ctx (Minus l r)= do { tl <- typeofM ctx l;
                              tr <- typeofM ctx r;
                              if(tl == TNum && tr == TNum)
                              then return TNum
                              else Nothing }

typeofM ctx (Mult l r)= do { tl <- typeofM ctx l;
                             tr <- typeofM ctx r;
                             if(tl == TNum && tr == TNum)
                             then return TNum
                             else Nothing }

typeofM ctx (Div l r)= do { tl <- typeofM ctx l;
                            tr <- typeofM ctx r;
                            if(tl == TNum && tr == TNum)
                            then return TNum
                            else Nothing }

typeofM ctx (Bind i v b) = do { tv <- typeofM ctx v;
                                typeofM ((i,tv):ctx) b }

typeofM ctx (Lambda i t b) = do { r <- typeofM ((i,t):ctx) b;
                                  return (t:->:r) }
typeofM ctx (App f a) = do { (d:->:r) <- typeofM ctx f;
                             at <- typeofM ctx a;
                             if(at==d)
                             then return r
                             else Nothing }

typeofM ctx (Id name) = (lookup name ctx)

typeofM ctx (And l r) = do { tl <- typeofM ctx l;
                             tr <- typeofM ctx r;
                             if(tl == TBool && tr == TBool)
                             then return TBool
                             else Nothing }

typeofM ctx (Or l r) = do { tl <- typeofM ctx l;
                            tr <- typeofM ctx r;
                            if(tl == TBool && tr == TBool)
                            then return TBool
                            else Nothing }

typeofM ctx (Leq a b) = do { ta <- typeofM ctx a;
                             tb <- typeofM ctx b;
                             if(ta == TNum && tb == ta)
                             then return TBool
                             else Nothing }

typeofM ctx (IsZero x) = do { tx <- typeofM ctx x;
                              if(tx == TNum)
                              then return TBool
                              else Nothing }

typeofM ctx (If c t e) = do { tc <- typeofM ctx c;
                              tt <- typeofM ctx t;
                              te <- typeofM ctx e;
                              if(tc == TBool)
                              then  if( te == tt)
                                    then return te
                                    else Nothing
                              else Nothing }

-- Interpreter

interp :: FBAE -> (Maybe FBAEVal)
interp myFBAE = do { typeofM [] myFBAE;
                     evalM [] myFBAE }















--
