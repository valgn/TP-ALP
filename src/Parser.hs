module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        , "--"
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
-- chainl1 permite parsear operadores asociativos a izquierda, pero evitando la recursión a izquierda
intexp :: Parser (Exp Int)
intexp = chainl1 intterm addop

intterm :: Parser (Exp Int)  -- Modified
intterm = chainl1 factor mulop

factor :: Parser (Exp Int) -- M
factor = do {reservedOp lis "-" ; f <- factor ; return (UMinus f)}
  <|> parens lis intexp
  <|> do {val <- integer lis ; return (Const (fromInteger val))}
  <|> do {var <- identifier lis ; 
           (reservedOp lis "++" >> return (VarInc var)) 
       <|> (reservedOp lis "--" >> return (VarDec var))
       <|> return (Var var)
         }
         
addop :: Parser (Exp Int -> Exp Int -> Exp Int)
addop = (reservedOp lis "+" >> return Plus)
  <|> (reservedOp lis "-" >> return Minus)

mulop :: Parser (Exp Int -> Exp Int -> Exp Int) -- M
mulop = (reservedOp lis "*" >> return Times)
  <|> (reservedOp lis "/" >> return Div)

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

-- Precedencia: (== != < >) ... ! ... && ... ||
boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolterm orop

boolterm :: Parser (Exp Bool)
boolterm = chainl1 boolfactor andop

boolfactor :: Parser (Exp Bool) -- M
boolfactor = do { reservedOp lis "!" ; f <- boolfactor ; return (Not f) }
  <|> try (parens lis boolexp)
  <|> try comp
  <|> (reservedOp lis "true" >> return BTrue) <|> (reservedOp lis "false" >> return BFalse)

comp :: Parser (Exp Bool)
comp = do expr1 <- intexp;
          op <- compop;
          expr2 <- intexp;
          return (op expr1 expr2)

-- Operaciones booleanas Agregadas
orop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orop = reservedOp lis "||" >> return Or

andop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andop = reservedOp lis "&&" >> return And

compop :: Parser (Exp Int -> Exp Int -> Exp Bool)
compop = (reservedOp lis "==" >> return Eq)
      <|>(reservedOp lis "!=" >> return NEq)
      <|>(reservedOp lis "<"  >>  return Lt)
      <|>(reservedOp lis ">"  >> return Gt)
-----------------------------------
--- Parser de comandos
-----------------------------------
-- Precedencia = ;

comm :: Parser Comm
comm = chainl1 commterm semcolop

semcolop :: Parser (Comm -> Comm -> Comm)
semcolop = (reservedOp lis ";" >> return Seq)

commterm :: Parser Comm
commterm = do { var <- identifier lis ; reservedOp lis "=" ; expr <- intexp ; return (Let var expr) }
        <|> do reserved lis "if" ; 
               bexp <- boolexp ; 
               c1 <- braces lis comm ; 
               (do {reserved lis "else" ; c2 <- braces lis comm ; return (IfThenElse bexp c1 c2)}) <|> return (IfThen bexp c1) 
        <|> do reserved lis "repeat" ; 
               c1 <- braces lis comm ; 
               reserved lis "until" ; 
               bexp <- boolexp ; 
               return (RepeatUntil c1 bexp) 
        <|> (reserved lis "skip" >> return Skip)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
