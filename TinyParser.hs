{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant bracket" #-}
import Parsing
import Control.Monad.RWS (MonadWriter(pass))

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
         do symbol "true"
            return TT
         +++
         do symbol "false"
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

-- Parses a string into an expression (Exp).
eparse :: String -> Exp
eparse xs = case (parse expr xs) of
             -- If the parsing is successful and there is no remaining input, return the parsed expression.
             [(n,[])] -> n
             -- If there is unused input, raise an error indicating the unused input.
             [(_,out)] -> error ("ERROR: UNUSED INPUT: `" ++ out ++ "`")
             -- If the parsing fails, raise an error indicating invalid input.
             [] -> error "ERROR: INVALID INPUT"

-- Parses a string into a command (Cmd).
cparse :: String -> Cmd
cparse xs = case (parse cmdrSeq xs) of
             -- If the parsing is successful and there is no remaining input, return the parsed command.
             [(n,[])] -> n
             -- If there is unused input, raise an error indicating the unused input.
             [(_,out)] -> error ("ERROR: UNUSED INPUT: `" ++ out ++ "`")
             -- If the parsing fails, raise an error indicating invalid input.
             [] -> error "ERROR: INVALID INPUT"

-- Test programs
-- Run `cparse [programName]` in GHCi to parse the program.

-- Correctly failing programs
failProgram1 = "x:=0;while (x=0) do (false);output x"
failProgram2 = "x:=1;y:=(output x);if (x=1) do y"
failProgram3 = "x:=0;if (x=1 then (output x) else (output 0)"

-- Correctly passing programs
gordonProgram = "sum:=0;x:=read;while not (x=true) do sum:=sum+x;x:=read;output sum"
passProgram1 = "x:=0;y:=1;count:=read;if (count=true) then (output x) else (output y)"
passProgram2 = "y:=1;num:=read;count:=0;while (num=0) do (count:=count+y;num:=read);output count"
passProgram3 = "x:=0;if (x=0) then (if (read=1) then (output 1) else (output 0)) else output x"