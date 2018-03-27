{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)

type Env = [(String,BBAE)]

type Cont = [(String,TBBAE)]

subst:: String -> BBAE -> BBAE -> BBAE
subst i v (Num n) = (Num n)
subst i v (Boolean b) = (Boolean b)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (If c t e) = (If (subst i v c) (subst i v t) (subst i v e))
subst i v (IsZero e) = (IsZero (subst i v e))
subst i v (Id i') = if (i == i')
                     then v
                     else (Id i')
subst i v (Bind i' v' b') = if ( i == i')
                              then (Bind i' (subst i v v') b')
                              else (Bind i' (subst i v v') (subst i v b'))


evalS :: BBAE -> (Maybe BBAE)
evalS (Num n) = Just (Num n)
evalS (Boolean b) = Just (Boolean b)

evalS (Plus l r) = do { (Num l') <- (evalS l);
                        (Num r') <- (evalS r);
                        return (Num (l' + r')) }

evalS (Minus l r) = do { (Num l') <- (evalS l);
                         (Num r') <- (evalS r);
                         if ((l' - r') > 0)
                         then return (Num (l' - r'))
                         else Nothing }

evalS (And l r) = do { (Boolean l') <- (evalS l);
                       (Boolean r') <- (evalS r);
                       return (Boolean (l' && r')) }

evalS (Leq l r) = do { (Num l') <- (evalS l);
                       (Num r') <- (evalS r);
                       return (Boolean (l' <= r'))}

evalS (If c t e) =  do { x <- (evalS c);
                        if (x == (Boolean True))
                          then (evalS t)
                          else (evalS e) }

evalS (IsZero e) = do { x <- (evalS e);
                        return (Boolean (x == (Num 0))) }

evalS (Bind i v b) = do { v' <- (evalS v);
                          evalS (subst i v' b) }
evalS (Id var) = Nothing


evalM :: Env -> BBAE -> (Maybe BBAE)
evalM env (Num n) = Just (Num n)
evalM env (Boolean b) = Just (Boolean b)

evalM env (Plus l r) = do { (Num l') <- (evalM env l);
                            (Num r') <- (evalM env r);
                            return (Num (l' + r')) }

evalM env (Minus l r) = do { (Num l') <- (evalM env l);
                             (Num r') <- (evalM env r);
                             if ((l' - r')  > 0)
                             then return (Num (l' - r'))
                             else Nothing }

evalM env (And l r) = do { (Boolean l') <- (evalM env l);
                           (Boolean r') <- (evalM env r);
                           return (Boolean (l' && r')) }

evalM env (Leq l r) = do { (Num l') <- (evalM env l);
                           (Num r') <- (evalM env r);
                           return (Boolean (l' <= r')) }

evalM env (If c t e) = do { x <- (evalM env c);
                            if (x == (Boolean True))
                              then (evalM env t)
                              else (evalM env e) }

evalM env (IsZero e) = do { x <- (evalM env e);
                            return (Boolean (x == (Num 0))) }

evalM env (Bind i v b) = do { v' <- (evalM env v);
                              (evalM ((i,v'):env) b) }

evalM env (Id var) = (lookup var env)


testBBAE :: BBAE -> Bool
testBBAE bbae = ((evalS bbae) == (evalM [] bbae))

typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM cont (Num x) = (Just TNum)
typeofM cont (Boolean b) = Just TBool

typeofM cont (Plus l r) = do { l' <- (typeofM cont l) ;
                              r' <- (typeofM cont r) ;
                              if l'==TNum && r'==TNum
                              then return TNum
                              else Nothing }

typeofM cont (Minus l r) = do { l' <- (typeofM cont l) ;
                               r' <- (typeofM cont r) ;
                               if l'==TNum && r'==TNum
                               then return TNum
                               else Nothing }

typeofM cont (And l r) = do { TBool <- (typeofM cont l) ;
                             TBool <- (typeofM cont r) ;
                             return TBool }

typeofM cont (Leq l r) = do { TNum <- (typeofM cont l) ;
                             TNum <- (typeofM cont r) ;
                             return TBool }

typeofM cont (If c t e) = do { c' <- (typeofM cont c) ;
                              t' <- (typeofM cont t) ;
                              e' <- (typeofM cont e) ;
                              if (t'==e')
                                 then return t'
                                 else Nothing }

typeofM cont (IsZero v) = do { TNum <- (typeofM cont v) ;
                              return TBool }

typeofM cont (Bind i v b) = do { v' <- typeofM cont v;
                                typeofM ((i,v'):cont) b }

typeofM cont (Id var) = (lookup var cont)

evalT :: BBAE -> (Maybe BBAE)
evalT bbae = do { t <- (typeofM [] bbae);
                  (evalM [] bbae) }
