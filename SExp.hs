module SExp where

import Text.PrettyPrint

data SExp
  = SymS Symbol
  | LitS Literal
  | ListS [SExp]
  | DirectiveS ReaderDirective
  | NilS
  deriving (Eq)

instance Show SExp where
  show = showSexp

data Literal
  = IntL Integer
  | StringL String
  deriving (Show, Eq)

type ReaderDirective = ()

type Symbol = String

pprint :: SExp -> Doc
pprint (ListS es) = parens $ sep $ map pprint es
pprint (LitS lit) = case lit of
  StringL s -> text $ show s
  IntL n -> integer n
pprint (SymS sym) = text sym
pprint (DirectiveS d) = text $ "DIRECTIVE<" ++ show d ++ ">"
pprint NilS = text "<nil>"

showSexp :: SExp -> String
showSexp = render . pprint

sym :: String -> SExp
sym = SymS

str :: String -> SExp
str s = LitS $ StringL s

asStr :: SExp -> String
asStr (LitS (StringL str)) = str
asStr _ = ""

car :: SExp -> SExp
car (ListS (x:y)) = x
car _ = NilS

cdr :: SExp -> SExp
cdr (ListS (x:y)) = ListS y
cdr _ = NilS

cons :: SExp -> SExp -> SExp
cons a d = ListS (a : unListS d)

unListS :: SExp -> [SExp]
unListS (ListS x) = x
unListS _ = []

-- vim: sw=2 ts=2 sts=2
