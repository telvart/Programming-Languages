{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- AST Definition

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)

-- AST Pretty Printer

pprint :: ABE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ " + " ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ " - " ++ pprint m ++ ")"
pprint (Mult n m) = "(" ++ pprint n ++ " * " ++ pprint m ++ ")"
pprint (Div n m) = "(" ++ pprint n ++ " / " ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ " && " ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ " <= " ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero " ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"


-- Parser (Requires ParserUtils and Parsec)

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

expr :: Parser ABE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "*" Plus AssocLeft
            , inFix "/" Minus AssocLeft ]
          , [ inFix "+" Plus AssocLeft
            , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

term = parens lexer expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr

-- Parser invocation

parseABE = parseString expr

liftNum :: (Int -> Int -> Int) -> ABE -> ABE -> ABE
liftNum f (Num l) (Num r) = (Num (f l r))

liftNumBool :: (Int -> Int -> Bool) -> ABE -> ABE -> ABE
liftNumBool f (Num l) (Num r) = (Boolean (f l r))

liftBool :: (Bool -> Bool -> Bool) -> ABE -> ABE -> ABE
liftBool f (Boolean l) (Boolean r) = (Boolean (f l r))

main = print "Hello"

-- Evaluation Functions

evalM :: ABE -> (Maybe ABE)
evalM (Num n) = Just (Num n)
evalM (Boolean b) = Just (Boolean b)
evalM (Plus l r) = do { x <- (evalM l);
                        y <- (evalM r);
                        return (liftNum (+) x y) }
evalM (Minus l r) = do { x <- (evalM l);
                         y <- (evalM r);
                         return (liftNum (-) x y) }
evalM (And l r) = do { x <- (evalM l);
                       y <- (evalM r);
                       return (liftBool (&&) x y) }
evalM (Leq l r) = do { x <- (evalM l);
                       y <- (evalM r);
                       return (liftNumBool (<=) x y) }
evalM (IsZero e) = do { x <- (evalM e);
                        return (Boolean (x == (Num 0))) }
evalM (If c t e) = do { x <- (evalM c);
                        (if (x == (Boolean True)) then (evalM t) else (evalM e)) }
evalM (Mult l r) = do { x <- (evalM l);
                        y <- (evalM r);
                        return (liftNum (*) x y)}
evalM (Div l r) = Nothing--do { x <- (evalM l);
                    --   y <- (evalM r);
                      -- Just (Num 1)}
                       --if (r == (Num 0)) then Nothing
                       --else (liftNum (div) x y)}


evalErr :: ABE -> (Maybe ABE)
evalErr (Num n) = Just (Num n)

evalErr (Boolean b) = Just (Boolean b)

evalErr (Plus l r) = do { x <- (evalErr l);
                          y <- (evalErr r);
                          case x of
                            (Num n1) -> case y of
                              (Num n2) -> Just (Num (n1 + n2))
                              _ -> Nothing
                            _ -> Nothing }

evalErr (Minus l r) = do { x <- (evalErr l);
                           y <- (evalErr r);
                           case x of
                             (Num n1) -> case y of
                                (Num n2) -> Just (Num (n1 - n2))
                                _ -> Nothing
                             _ -> Nothing }

evalErr (And l r) = do { x <- (evalErr l);
                         y <- (evalErr r);
                         case x of
                          (Boolean b1) -> case y of
                            (Boolean b2) -> Just (Boolean (b1 && b2))
                            _ -> Nothing
                          _ -> Nothing }

evalErr (Leq l r) = do { x <- (evalErr l);
                         y <- (evalErr r);
                         case x of
                          (Num n1) -> case y of
                            (Num n2) -> Just (Boolean (n1 <= n2))
                            _ -> Nothing
                          _ -> Nothing }

evalErr (IsZero e) = do { x <- (evalErr e);
                          case x of
                            (Num n) -> Just (Boolean (n == 0))
                            _ -> Nothing }

evalErr (If c t e) = do { x <- (evalErr c);
                          case x of
                            (Boolean b) -> if b then (evalErr t) else (evalErr e)
                            _ -> Nothing }

evalErr (Mult l r) = do { x <- (evalErr l);
                          y <- (evalErr r);
                          case x of
                            (Num n1) -> case y of
                              (Num n2) -> Just (Num (n1 * n2))
                              _ -> Nothing
                            _ -> Nothing }

evalErr (Div l r) =  do { x <- (evalErr l);
                          y <- (evalErr r);
                          case x of
                            (Num n1) -> case y of
                              (Num n2) -> if (n2 == 0) then Nothing
                                          else Just (Num(div n1 n2))
                              _ -> Nothing
                            _ -> Nothing }

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM (Num n) = Just TNum
typeofM (Boolean b) = Just TBool

typeofM (Plus l r) = do { x <- (typeofM l);
                          y <- (typeofM r);
                          if (x == TNum && y == TNum)
                            then Just TNum
                            else Nothing }

typeofM (Minus l r) = do { x <- (typeofM l);
                           y <- (typeofM r);
                           if (x == TNum && y == TNum)
                             then Just TNum
                             else Nothing }

typeofM (And l r) = do { x <- (typeofM l);
                         y <- (typeofM r);
                         if (x == TBool && y == TBool)
                            then Just TBool
                            else Nothing }

typeofM (Leq l r) = do { x <- (typeofM l);
                         y <- (typeofM r);
                         if (x == TNum && y == TNum)
                            then Just TBool
                            else Nothing }

typeofM (IsZero t) = do { x <- (typeofM t);
                          if (x == TNum)
                            then Just TBool
                            else Nothing }

typeofM (If c t e) = do { x <- (typeofM c);
                          y <- (typeofM t);
                          z <- (typeofM e);
                          if (x == TBool && y == z)
                            then Just y
                            else Nothing}

typeofM (Mult l r) = do { x <- (typeofM l);
                          y <- (typeofM r);
                          if ( x == TNum && y == TNum)
                            then Just TNum
                            else Nothing }
typeofM (Div l r) = do { x <- (typeofM l);
                         y <- (typeofM r);
                         if (x == TNum && y == TNum)
                          then Just TNum
                          else Nothing }
-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE
evalTypeM _ = Nothing

-- Optimizer
optimize :: ABE -> ABE
optimize e = e

interpOptM :: ABE -> Maybe ABE
interpOptM _ = Nothing

interpABE :: String -> Maybe ABE
interpABE s = (evalErr (parseABE s))
