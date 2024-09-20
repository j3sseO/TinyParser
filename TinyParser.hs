{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant bracket" #-}
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

-- Parsing Expressions

-- `expr` is the top-level parser for expressions. It parses the following constructs:
-- - Addition: `e1 + e2`
-- - Equality: `e1 = e2`
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

-- `term` is the parser for individual terms. It parses the following constructs:
-- - Negation: `not e`

term :: Parser Exp
term = do symbol "not"
          e <- expr
          return (Not e)
       +++
       factor

-- `factor` is the parser for individual factors. It parses the following constructs:
-- - Constants: `0`, `1`, `tt`, `ff`
-- - Read: `read`
-- - Identifier: `x`
-- - Parenthesized expressions: `(e)`
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

-- Parsing Commands

-- `cmdrSeq` is the top-level parser for commands. It parses a sequence of commands separated by semicolons.
cmdrSeq :: Parser Cmd
cmdrSeq = do c1 <- cmdrInd
             do symbol ";"
                c2 <- cmdrSeq
                return (Seq c1 c2)
          +++
          cmdrInd

-- `cmdrInd` is the parser for individual commands. It parses the following constructs:
-- - If-Then-Else: `if e then c1 else c2`
-- - While-Do: `while e do c`
-- - Assignment: `x := e`
-- - Output: `output e`
-- - Command sequence in parentheses: `(c)`
cmdrInd :: Parser Cmd
cmdrInd = do string "if "
             e <- expr
             do string "then "
                c1 <- cmdrSeq
                do string "else "
                   c2 <- cmdrSeq
                   return (IfThenElse e c1 c2)
          +++
          do string "while "
             e <- expr
             do string "do "
                c <- cmdrSeq
                return (WhileDo e c)
          +++
          do s <- identifier
             do string ":="
                e <- expr
                return (Assign s e)
          +++
          do string "output "
             e <- expr
             return (Output e)
          +++
          do symbol "("
             c <- cmdrSeq
             do symbol ")"
                return c 
