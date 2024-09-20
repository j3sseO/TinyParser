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

