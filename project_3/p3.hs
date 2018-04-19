{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- CFAE AST and Type Definitions

data CFAE where
  Num :: Int -> CFAE
  Plus :: CFAE -> CFAE -> CFAE
  Minus :: CFAE -> CFAE -> CFAE
  Lambda :: String -> CFAE -> CFAE
  App :: CFAE -> CFAE -> CFAE
  Id :: String -> CFAE
  If0 :: CFAE -> CFAE -> CFAE -> CFAE
  deriving (Show,Eq)

type Env = [(String,CFAE)]

evalDynCFAE :: Env -> CFAE -> (Maybe CFAE)
evalDynCFAE env (Num n) = return (Num n)

evalDynCFAE env (Plus l r) = do { (Num l') <- (evalDynCFAE env l);
                                  (Num r') <- (evalDynCFAE env r);
                                  return (Num (l' + r')) }

evalDynCFAE env (Minus l r) = do { (Num l') <- (evalDynCFAE env l);
                                   (Num r') <- (evalDynCFAE env r);
                                   if ((l' - r')  > 0)
                                   then return (Num (l' - r'))
                                   else Nothing }

evalDynCFAE env (Lambda iden body) = return (Lambda iden body)

evalDynCFAE env (App func arg) = do { (Lambda i b) <- (evalDynCFAE env func);
                                       arg' <- (evalDynCFAE env arg);
                                       evalDynCFAE ((i,arg'):env) b }

evalDynCFAE env (Id name) = (lookup name env)

evalDynCFAE env (If0 cond tru els) = do { (Num n) <- (evalDynCFAE env cond);
                                          if (n == 0)
                                          then (evalDynCFAE env tru)
                                          else (evalDynCFAE env els) }

data CFAEValue where
  NumV :: Int -> CFAEValue
  ClosureV :: String -> CFAE -> Env' -> CFAEValue
  deriving (Show,Eq)

type Env' = [(String,CFAEValue)]

evalStatCFAE :: Env' -> CFAE -> (Maybe CFAEValue)
evalStatCFAE env (Num n) = return (NumV n)
evalStatCFAE env (Plus l r) = do { (NumV l') <- (evalStatCFAE env l);
                                   (NumV r') <- (evalStatCFAE env r);
                                   return (NumV (l' + r')) }

evalStatCFAE env (Minus l r) = do { (NumV l') <- (evalStatCFAE env l);
                                    (NumV r') <- (evalStatCFAE env r);
                                    if ((l' - r')  > 0)
                                    then return (NumV (l' - r'))
                                    else Nothing }

evalStatCFAE env (Lambda iden body) = return (ClosureV iden body env)

evalStatCFAE env (App func arg) = do { arg' <- (evalStatCFAE env arg);
                                       (ClosureV i b env') <- (evalStatCFAE env func);
                                       (evalStatCFAE ((i,arg'):env') b) }

evalStatCFAE env (Id name) = (lookup name env)

evalStatCFAE env (If0 cond tru els) =  do { (NumV n) <- (evalStatCFAE env cond);
                                          if (n == 0)
                                          then (evalStatCFAE env tru)
                                          else (evalStatCFAE env els) }


data CFBAE where
  Num' :: Int -> CFBAE
  Plus' :: CFBAE -> CFBAE -> CFBAE
  Minus' :: CFBAE -> CFBAE -> CFBAE
  Lambda' :: String -> CFBAE -> CFBAE
  App' :: CFBAE -> CFBAE -> CFBAE
  Bind' :: String -> CFBAE -> CFBAE -> CFBAE
  Id' :: String -> CFBAE
  If0' :: CFBAE -> CFBAE -> CFBAE -> CFBAE
  deriving (Show,Eq)

elabCFBAE :: CFBAE -> CFAE
elabCFBAE (Num' n) = (Num n)
elabCFBAE (Plus' l r) = (Plus (elabCFBAE l) (elabCFBAE r))
elabCFBAE (Minus' l r) = (Minus (elabCFBAE l) (elabCFBAE r))
elabCFBAE (Lambda' iden bod) = (Lambda iden (elabCFBAE bod))
elabCFBAE (App' func arg) = (App (elabCFBAE func) (elabCFBAE arg))
elabCFBAE (Bind' i v b) = (App (Lambda i (elabCFBAE v)) (elabCFBAE b))
elabCFBAE (Id' name) = (Id name)
elabCFBAE (If0' c t e) = (If0 (elabCFBAE c) (elabCFBAE t) (elabCFBAE e))

evalCFBAE :: Env' -> CFBAE -> (Maybe CFAEValue)
evalCFBAE env cfbae = (evalStatCFAE env (elabCFBAE cfbae))
