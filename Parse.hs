module Parse where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe

import SExp
import Syntax

type Parse = Either String 

parseModule :: SExp -> Parse Module
parseModule src = mapM parseDecl =<< expectList "module" src

parseDecl :: SExp -> Parse Decl
parseDecl src = do
    list <- expectList "declaration" src
    case list of
        SymS "def":nameSym:body -> do
            name <- expectSym "top-level variable" nameSym
            VarD (stringVar name) <$> parseDo body
        _ -> Left $ "unknown type of declaration: " ++ show src

parseExpr :: SExp -> Parse Expr
parseExpr src = case src of
    SymS sym -> return $ Ref $ stringVar sym
    LitS (SExp.IntL k) -> return $ Lit (Syntax.IntL $ fromIntegral k)
    LitS (SExp.StringL k) -> return $ Lit (Syntax.StringL k)
    ListS (hd:rest) -> case hd of
        -- "do" translates to Let, "let" translates to Letrec.
        SymS "do" -> parseDo rest
        SymS "let" -> parseLet rest

        SymS "\\" -> parseLam rest
        SymS "case" -> parseCase rest
        SymS "<-" -> parseAssign rest
        SymS "while" -> parseWhile rest
        SymS "js" -> parseJS rest
        SymS "typed" -> parseTyped rest
        _ -> App <$> parseExpr hd <*> mapM parseExpr rest
    _ -> Left $ "unknown type of expression: " ++ show src

parseDo, parseLet, parseLam, parseCase, parseAssign, parseWhile, parseJS, parseTyped :: [SExp] -> Parse Expr
parseDo [] = Left "empty do expression"
parseDo list = do
    stmts <- mapM parseStmt list
    let (si, sl) = (init stmts, last stmts)
    lst <- case sl of
        ExpStmt exp -> return exp
        _ -> Left $ "do expression must end with an expression: " ++ show list
    return $ foldr toLet lst si
    where
        toLet stmt body = case stmt of
            ExpStmt exp -> Let UnitP exp body
            BindStmt pat exp -> Let pat exp body

data Stmt = ExpStmt Expr | BindStmt Pat Expr

parseStmt :: SExp -> Parse Stmt
parseStmt src = parseBindStmt src `orElse` (ExpStmt <$> parseExpr src)

parseBindStmt :: SExp -> Parse Stmt
parseBindStmt src = do
    list <- expectList "bind statement" src
    case list of
        [pat,SymS "=", body] -> BindStmt <$> parsePat pat <*> parseExpr body
        _ -> Left "malformed bind statement"

parseLet _ = Left $ "sorry, let expression is not supported yet"

parseLam list = case break isRightArrow list of
    (paramSrcs, _:body) -> do
        pats <- mapM parsePat paramSrcs
        params <- forM pats $ \pat -> case pat of
            VarP var -> return var
            _ -> Left $ "sorry, you can't bind a non-variable pattern in a lambda yet"
        Lam params <$> parseDo body
    _ -> Left $ "malformed lambda: " ++ show list
    where
        isRightArrow (SymS "->") = True
        isRightArrow _ = False

parseCase [] = Left "empty case"
parseCase (scr:branchSrcs) = Case <$> parseExpr scr <*> mapM parseBranch branchSrcs

parseBranch :: SExp -> Parse Branch
parseBranch (ListS (pat:body)) = (,) <$> parsePat pat <*> parseDo body
parseBranch list = Left $ "malformed branch: " ++ show list

parseAssign [pat, exp] = Assign <$> parsePat pat <*> parseExpr exp
parseAssign list = Left $ "malformed assignment: " ++ show list

parseWhile [] = Left "empty while"
parseWhile (cond:body) = While <$> parseExpr cond <*> parseDo body

parseJS [LitS (SExp.StringL js)] = return $ JS js
parseJS list = Left $ "malformed js embedding: " ++ show list

parseTyped [exp, ty] = Typed <$> parseExpr exp <*> parseType ty
parseTyped list = Left $ "malformed ::: " ++ show list

parsePat :: SExp -> Parse Pat
parsePat src = case src of
    SymS k
        | isConstructorSymbol k -> return $ ConstructorP (stringVar k) []
        | otherwise -> return $ VarP (stringVar k)
    LitS (SExp.IntL k) -> return $ IntP (fromIntegral k)
    LitS (SExp.StringL k) -> return $ StringP k
    ListS [] -> return UnitP
    ListS (SymS hd:args)
        | isConstructorSymbol hd -> ConstructorP (stringVar hd) <$> mapM parsePat args
    _ -> Left $ "malformed pattern: " ++ show src
    where
        isConstructorSymbol = maybe False isUpper . listToMaybe

parseType :: SExp -> Parse Type
parseType src = case src of
    SymS k -> return $ ConstT (stringVar k) -- no way to write a type variable, for now
    ListS (hd:args) -> AppT <$> parseType hd <*> mapM parseType args
    _ -> Left $ "malformed type expression: " ++ show src

expectList :: String -> SExp -> Parse [SExp]
expectList desc sexp = case sexp of
    ListS list -> return list
    _ -> Left $ desc ++ ": expecting a list but got " ++ show sexp

expectSym :: String -> SExp -> Parse String
expectSym desc sexp = case sexp of
    SymS sym -> return sym
    _ -> Left $ desc ++ ": expecting a symbol but got " ++ show sexp

orElse :: Parse a -> Parse a -> Parse a
orElse Left{} x = x
orElse x _ = x
