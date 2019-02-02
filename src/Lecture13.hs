--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 13: Reasoning about programs (cont.)                               --
--------------------------------------------------------------------------------

module Lecture13 where

--------------------------------------------------------------------------------
-- Expression language

data Expr = Val Int
          | Plus Expr Expr
          deriving Show

expr0 :: Expr
expr0 = Val 4

expr1 :: Expr
expr1 = Plus (Val 4) (Val 8)

expr2 :: Expr
expr2 = Plus (Plus (Val 4) (Val 15)) (Val 8)

--------------------------------------------------------------------------------
-- Interpreter

-- | `eval` @expr@ evaluates @expr@.
eval :: Expr -> Int
eval (Val n)    = n
eval (Plus l r) = eval l + eval r

--------------------------------------------------------------------------------
-- Instruction set

data Instr = PUSH Int
           | ADD
           deriving Show

type Program = [Instr]
type Stack   = [Int]

program0 :: Program
program0 = [PUSH 4]

program1 :: Program
program1 = [PUSH 4, PUSH 8, ADD]

program2 :: Program
program2 = [PUSH 4, PUSH 15, ADD, PUSH 8, ADD]

-- | `exec` @program stack@ executes @program@ with an initial @stack@.
exec :: Program -> Stack -> Stack
exec []                    s  = s
exec (PUSH n : p)          s  = exec p (n:s)
exec (ADD    : p) (y : x : s) = exec p (x+y:s)

--------------------------------------------------------------------------------
-- Compiler

-- | `comp` @expr@ compiles @expr@ into a program.
comp :: Expr -> Program
comp (Val n)    = [PUSH n]
comp (Plus l r) = comp l ++ comp r ++ [ADD]

--------------------------------------------------------------------------------
