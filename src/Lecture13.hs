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
