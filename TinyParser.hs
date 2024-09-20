{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import Parsing

-- Data types

type Ide = String

data Exp = Zero 
         | One 
         | TT 
         | FF 
         | Read 
         | I Ide 
         | Not Exp 
         | Equal Exp Exp 
         | Plus Exp Exp 
         deriving Show

data Cmd = Assign Ide Exp
          | Output Exp
          | IfThenElse Exp Cmd Cmd
          | WhileDo Exp Cmd
          | Seq Cmd Cmd
          deriving Show

expr :: Parser Exp
expr =  do e1 <- term
           do symbol "+"
              e2 <- expr
              return (Plus e1 e2)
        +++
        do e1 <- term
           do symbol "="
              e2 <- expr
              return (Equal e1 e2)
        +++
        term

term :: Parser Exp
term = do symbol "not"
          e <- expr
          return (Not e)
       +++
       factor

factor :: Parser Exp
factor = do symbol "0"
            return Zero
         +++
         do symbol "1"
            return One
         +++
         do symbol "tt"
            return TT
         +++
         do symbol "ff"
            return FF
         +++
         do symbol "read"
            return Read
         +++
         do i <- identifier
            return (I i)
         +++
         do symbol "("
            e <- expr
            symbol ")"
            return e

