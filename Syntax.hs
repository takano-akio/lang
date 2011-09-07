module Syntax where

import Data.ByteString.Char8 (ByteString, pack)

data Expr
    = App Expr [Expr]
    | Let Pat Expr Expr
    | Letrec [(Var, Expr)] Expr
    | Lam [Var] Expr
    | Case Expr [Branch]
    | Ref Var
    | Lit Lit
    | Assign Pat Expr
    | While Expr Expr
  -- | Return Expr
    | JS String
    | Typed Expr Type
    deriving (Show)

data Lit
    = IntL Int
    | StringL String
    deriving (Show)

type Branch = (Pat, Expr)

data Pat
    = VarP Var
    | ConstructorP Var [Pat]
    | IntP Int
    | StringP String
    | UnitP
    | WildcardP
    deriving (Show)

data Var = Var Int ByteString
    deriving Show

instance Eq Var where
    Var n x == Var m y = n == m && (n /= 0 || x == y)

stringVar :: String -> Var
stringVar x = Var 0 (pack x)

instance Ord Var where
    compare (Var 0 x) (Var 0 y) = compare x y
    compare Var{} (Var 0 y) = GT
    compare (Var 0 x) Var{} = LT
    compare (Var n _) (Var m _) = compare n m

type Module = [Decl]
data Decl
    = VarD Var Expr
    | DataD Var [Var] [(Var, [Type])]
    deriving (Show)

data Type
    = VarT Var
    | ConstT Var
    | AppT Type [Type]
    deriving (Show, Eq)
