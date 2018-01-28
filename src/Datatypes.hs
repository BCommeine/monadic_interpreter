module Datatypes where

  type Words = String

  data Direction    = Right
                    | Left
                    deriving(Eq, Show)

  data Value        = Wrong
                    | Num Int
                    | Bool Bool
                    | Dec Float
                    deriving(Eq)

  data Expression   = Var Words
                    | Con Int
                    | ReadSensor
                    | LightSensor Direction
                    | BoolVal Bool
                    | Decimal Float
                    --- Artithmic operators ---------------
                    | Add Expression Expression
                    | Mul Expression Expression
                    | Sub Expression Expression
                    | Div Expression Expression
                    --- Comparing operators ---------------
                    | Eq  Expression Expression
                    | Greater Expression Expression
                    | Lesser Expression Expression
                    --- Boolean operators ----------------
                    | And Expression Expression
                    | Or  Expression Expression
                    | Not Expression
                    deriving( Show, Eq)

  data Statement    = Assignement Words Expression
                    | If Expression Statement Statement
                    | While Expression Statement
                    | Seq [Statement]
                    | Print Expression
                    | Comment
                    | Drive Expression
                    | Turn Direction Expression
                    | Lights Expression Expression Expression Expression
                    deriving(Eq)
                    --Turn Expression

  instance (Show Value) where
    show Wrong    = "Wrong"
    show (Num i)  = "Int " ++ show i
    show (Bool i) = show i
    show (Dec i)   = "Decimal " ++ show i

  instance (Show Statement) where
    show (Assignement e f) = "Assign " ++ show e ++ show f
    show (If ex stmt1 stmt2)  = "If " ++ show ex ++ "then " ++ show stmt1 ++ "else " ++ show stmt2
    show (While ex stmt) = "While " ++ show ex ++ "Do " ++ show stmt
    show (Print i) = "Print " ++ show i
