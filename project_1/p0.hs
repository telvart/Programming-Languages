--NAME: Tim Elvart
--KUID: 2760606
--Date: February 6, 2018
--Brief: This program implements three interpreter styles for the AE
--language that we have been discussing in class.


{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Num :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }

lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)


term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.

--Cannot allow any negative input numbers?
main = do{
  print (evalAEMaybe (Div (Num 10) (Num 0)));
--  print (evalAE If0 )
  --exercise1
}

exercise1 = do{
  putStrLn "\nTesting evalAE\n";
  putStrLn "evalAE (Num 5) = ";
  print (evalAE (Num 5));
  putStrLn "evalAE (Plus (Num 2) (Num 6)) = ";
  print (evalAE (Plus (Num 2) (Num 6)));
  putStrLn "evalAE (Minus (Num 10) (Num 4)) = ";
  print (evalAE (Minus (Num 10) (Num 4)));
--  putStrLn "evalAE (Minus (Num 4) (Num 10)) = ";
--  print (evalAE (Minus (Num 4) (Num 10)))

}

evalAE :: AE -> Int
evalAE (Num n) =
  if (n >= 0) then n
  else error "!"
evalAE (Plus l r) =
  if ((evalAE l) + (evalAE r) < 0) then error "!"
  else (evalAE l) + (evalAE r)
evalAE (Minus l r) =
  if ((evalAE l) < (evalAE r)) then error "!"
  else (evalAE l) - (evalAE r)
evalAE (Mult l r) = (evalAE l) * (evalAE r)
evalAE (Div l r) =
  if ((evalAE r) == 0) then error "!"
  else div (evalAE l)(evalAE r)
evalAE (If0 c t e) = if((evalAE c) == 0) then (evalAE e)
                     else (evalAE t)

evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Num n) =
  if (n >= 0) then Just n
  else Nothing
evalAEMaybe (Plus l r) =
  case (evalAEMaybe l) of
    Just l2 -> case (evalAEMaybe r) of
      Just r2 -> Just (l2 + r2)
      Nothing -> Nothing
    Nothing -> Nothing

evalAEMaybe (Minus l r) =
  case (evalAEMaybe l) of
    Just l2 -> case (evalAEMaybe r) of
      Just r2 -> if (l2 < r2) then Nothing
                 else Just (l2 - r2)
      Nothing -> Nothing
    Nothing -> Nothing

evalAEMaybe (Mult l r) =
  case (evalAEMaybe l) of
    Just l2 -> case (evalAEMaybe r) of
      Just r2 -> Just (l2 * r2)
      Nothing -> Nothing
    Nothing -> Nothing

evalAEMaybe (Div l r) =
  case (evalAEMaybe l) of
    Just l2 -> case (evalAEMaybe r) of
      Just r2 -> if (r2 == 0) then Nothing
                 else Just (div l2 r2)
      Nothing -> Nothing
    Nothing -> Nothing

evalAEMaybe (If0 c t e) =
  case (evalAEMaybe c) of
    Just c2 -> if (c2 == 0 ) then (evalAEMaybe e) --dont need to check for nothings because e or t can be nothing themselves
               else (evalAEMaybe t)
    Nothing -> Nothing


evalM :: AE -> Maybe Int
evalM (Num n) = if(n >= 0) then Just n
                else Nothing
evalM (Plus l r) = do{
                x <- evalM l;
                y <- evalM r;
                return (x + y)
}

evalM (Minus l r) = do{
                x <- evalM l;
                y <- evalM r;
                return (x - y)
}

evalM (Mult l r) = do{
                x <- evalM l;
                y <- evalM r;
                return (x * y)
}

evalM (Div l r) = do{
                x <- evalM l;
                y <- evalM r;
                if (y == 0) then Nothing
                else return (div x y)
}

evalM (If0 c t e) = do{
                x <- (evalM c);
                if (x == 0) then (evalM e)
                else (evalM t)
}

interpAE :: String -> Maybe Int
interpAE s = (evalM (parseAE s))
