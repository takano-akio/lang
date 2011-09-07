{-# LANGUAGE OverloadedStrings #-}
module Inference where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO.Unsafe
import Data.IORef
import qualified Data.ByteString.Char8
import Control.Applicative
import Data.Maybe

import Syntax

data TypeSchema = Forall [Var] Type
type TEnv = M.Map Var TypeSchema
type UEnv = M.Map Var Type -- unification environment

type Typecheck = StateT UEnv IO

runTypecheck :: Typecheck a -> IO a
runTypecheck tc = evalStateT tc M.empty

monomorphic :: Type -> TypeSchema
monomorphic = Forall []

typecheckModule :: TEnv -> Module -> Typecheck ()
typecheckModule tenv decls = do
    _ <- typecheckRecursiveGroup tenv binds
    return ()
    where
        binds = do
            VarD v e <- decls
            return (v, e)

typecheckExpr :: TEnv -> Expr -> Typecheck Type
typecheckExpr tenv expr = case expr of
    App fun args -> do
        funType <- typecheckExpr tenv fun
        argTypes <- mapM (typecheckExpr tenv) args
        resultType <- mkUniqueTV
        unify funType $ funT argTypes resultType
        return resultType
    Let pat defn body -> do
        defnType <- typecheckExpr tenv defn
        (patType, bound) <- typecheckPat tenv pat
        unify defnType patType
        typecheckExpr (M.union tenv $ fmap monomorphic bound) body
    Letrec binds body -> do
        additionalEnv <- typecheckRecursiveGroup tenv binds
        let finalEnv = M.union tenv additionalEnv
        typecheckExpr finalEnv body
    Lam params body -> do
        paramTypes <- mapM (const mkUniqueTV) params
        let bodyEnv = M.union tenv $ M.fromList $ zip params (map monomorphic paramTypes)
        resultType <- typecheckExpr bodyEnv body
        return $ funT paramTypes resultType
    Case scrt branches -> do
        scrtType <- typecheckExpr tenv scrt
        resultType <- mkUniqueTV
        forM_ branches $ \b -> do
            (patType, bodyType) <- typecheckBranch tenv b
            unify scrtType patType
            unify resultType bodyType
        return resultType
    Ref var -> case M.lookup var tenv of
        Nothing -> fail $ "unbound variable: " ++ show var
        Just ty -> instantiate ty
    Lit (IntL _) -> return intT
    Lit (StringL _) -> return stringT
    Assign pat body -> do
        bodyType <- typecheckExpr tenv body
        (patType, binds) <- typecheckPat tenv pat
        unify bodyType patType
        forM_ (M.toList binds) $ \(var, ty) -> case M.lookup var tenv of
            Nothing -> fail "attempt to assign to nonexistent variable"
            Just (Forall [] realTy) -> unify ty realTy
            Just _ -> fail "attempt to assign to a polymorphic variable"
        return unitT
    While cond body -> do
        unify boolT =<< typecheckExpr tenv cond
        unify unitT =<< typecheckExpr tenv body
        return unitT
    JS _ -> mkUniqueTV
    Typed body ty -> do
        inferred <- typecheckExpr tenv body
        unify inferred ty
        return inferred

typecheckRecursiveGroup :: TEnv -> [(Var, Expr)] -> Typecheck TEnv
typecheckRecursiveGroup tenv binds = do
    let (vars, defns) = unzip binds
    temporaryVarTypes <- mapM (const mkUniqueTV) binds
    let temporaryEnv = M.union tenv $ M.fromList $
            zip vars (map monomorphic temporaryVarTypes)
    forM_ (zip defns temporaryVarTypes) $ \(defn, ty) ->
        unify ty =<< typecheckExpr temporaryEnv defn
    finalVarTypes <- forM temporaryVarTypes $ \tmpType -> do
        tmpType' <- derefAll tmpType
        return $! generalize tenv tmpType'
    return $! M.fromList $ zip vars finalVarTypes

generalize :: TEnv -> Type -> TypeSchema
generalize tenv ty = Forall free ty
    where
        !free = S.toList $ independentVariables tenv ty

independentVariables :: TEnv -> Type -> S.Set Var
independentVariables tenv ty = tyVariables `S.difference` envVariables
    where
        tyVariables = typeVariables ty
        envVariables = S.unions $ map freeVariables $ M.elems tenv

freeVariables :: TypeSchema -> S.Set Var
freeVariables (Forall vs ty) = typeVariables ty `S.difference` S.fromList vs

typeVariables :: Type -> S.Set Var
typeVariables ty = case ty of
    VarT var -> S.singleton var
    ConstT {} -> S.empty
    AppT f xs -> S.unions $ map typeVariables (f:xs)

instantiate :: TypeSchema -> Typecheck Type
instantiate (Forall vs body) = do
    argTypes <- mapM (const mkUniqueTV) vs
    return $! substitute (M.fromList $ zip vs argTypes) body

substitute :: M.Map Var Type -> Type -> Type
substitute trans ty = case ty of
    VarT var -> fromMaybe ty $ M.lookup var trans
    ConstT _ -> ty
    AppT f xs -> AppT (substitute trans f) (map (substitute trans) xs)

typecheckBranch :: TEnv -> Branch -> Typecheck (Type, Type)
typecheckBranch tenv (pat, body) = do
    (patType, extraEnv) <- typecheckPat tenv pat
    bodyType <- typecheckExpr (M.union tenv $ fmap monomorphic extraEnv) body
    return (patType, bodyType)

typecheckPat :: TEnv -> Pat -> Typecheck (Type, M.Map Var Type)
typecheckPat tenv pat = case pat of
    VarP var -> do
        resultType <- mkUniqueTV
        return (resultType, M.singleton var resultType)
    ConstructorP cons args -> do
        resultType <- mkUniqueTV
        (argTypes, argEnvs) <- unzip <$> mapM (typecheckPat tenv) args
        case M.lookup cons tenv of
            Nothing -> fail "pattern match on unknwon constructor"
            Just consSchema -> do
                consType <- instantiate consSchema
                unify consType $ funT argTypes resultType
        return (resultType, M.unions argEnvs)
    IntP _ -> return (intT, M.empty)
    StringP _ -> return (stringT, M.empty)
    UnitP -> return (unitT, M.empty)
    WildcardP -> do
        resultType <- mkUniqueTV
        return (resultType, M.empty)

unify :: Type -> Type -> Typecheck ()
unify (VarT x) ty = unifyVar x ty
unify ty (VarT x) = unifyVar x ty
unify (AppT f xs) ty = case ty of
    AppT g ys
        | length xs == length ys -> do
            unify f g
            sequence_ $ zipWith unify xs ys
    _ -> fail "unification failed"
unify (ConstT v) ty = case ty of
    ConstT w
        | v == w -> return ()
    _ -> fail $ "unification on const failed: " ++ show v ++ " <-> " ++ show ty

unifyVar :: Var -> Type -> Typecheck ()
unifyVar x ty = do
    xty <- deref x
    case xty of
        VarT v -> do
            ty' <- derefAll ty
            when (ty' /= VarT v) $ do
                when (occurs v ty') $ fail $
                    "occurs check failed: cannot unify " ++ show (v, ty')
                addToUEnv v ty
        _ -> unify xty ty

occurs :: Var -> Type -> Bool
occurs v ty = case ty of
    VarT u -> v == u
    ConstT{} -> False
    AppT f xs -> occurs v f || any (occurs v) xs

addToUEnv :: Var -> Type -> Typecheck ()
addToUEnv var ty = do
    uenv <- get
    put $! M.insert var ty uenv

deref :: Var -> Typecheck Type
deref x = do
    uenv <- get
    case M.lookup x uenv of
        Nothing -> return $ VarT x
        Just (VarT y) -> deref y
        Just ty -> return ty

derefAll :: Type -> Typecheck Type
derefAll (VarT v) = do
    uenv <- get
    case M.lookup v uenv of
        Nothing -> return $ VarT v
        Just ty -> derefAll ty
derefAll ty@ConstT{} = return ty
derefAll (AppT f xs) = AppT <$> derefAll f <*> mapM derefAll xs

funT :: [Type] -> Type -> Type
funT argTypes resultType = AppT (ConstT v_fun) (argTypes ++ [resultType])

intT :: Type
intT = ConstT v_int

stringT :: Type
stringT = ConstT v_string

unitT :: Type
unitT = ConstT v_unit

boolT :: Type
boolT = ConstT v_bool

v_fun = Var (-1) "->"
v_int = Var (-2) "int"
v_string = Var (-3) "string"
v_unit = Var (-4) "()"
v_bool = Var (-5) "bool"

mkUniqueTV :: Typecheck Type
mkUniqueTV = fmap VarT $ liftIO mkUniqueVar

mkUniqueVar :: IO Var
mkUniqueVar = do
    n <- mkUniqueInt
    return $ Var n ""

mkUniqueInt :: IO Int
mkUniqueInt = do
    v <- readIORef uniqueSupply
    writeIORef uniqueSupply $! v + 1
    return v

{-# NOINLINE uniqueSupply #-}
uniqueSupply :: IORef Int
uniqueSupply = unsafePerformIO $ newIORef 0

---- tests

tc_var n = Var n ""
tc_ref n = Ref (tc_var n)
tc_expr1 = Lam [tc_var 0] (tc_ref 0) -- \x -> x
tc_expr2 = Lam [tc_var 0] (App (tc_ref 0) [tc_ref 0]) -- \x -> x x
tc_expr3 = Lam [tc_var 0, tc_var 1] (tc_ref 0) -- \x y -> x
tc_expr4 = Lam [tc_var 0, tc_var 1] (App (tc_ref 1) [tc_ref 0]) -- \x y -> y x
tc_expr5 = Lam [tc_var 0] (App (tc_ref 0) [Lit (IntL 100)]) -- \x -> x 100
tc_expr6 = Lam [tc_var 0] (App (Lit (IntL 100))[tc_ref 0] ) -- \x -> 100 x
tc_expr7 = Lam [tc_var 0] (Typed (App (JS "console.log")[tc_ref 0]) unitT ) -- \x -> JS"console.log" x :: ()
tc_expr8 = Lam [tc_var 0] (Case (tc_ref 0) [(IntP 0, (Lit (IntL 100))), (WildcardP, tc_ref 0)])
    -- \x -> case x of 0 -> 100; _ -> x
tc_expr9 = Lam [tc_var 0] (Case (tc_ref 0) [(VarP (tc_var 1), tc_ref 1)])
    -- \x -> case x of y -> y
tc_expr10 = Lam [tc_var 0] (Assign (VarP (tc_var 0)) (Lit (IntL 3)))
    -- \x -> x := 3
tc_expr11 = Lam [tc_var 0, tc_var 1] $ Let (VarP (tc_var 2)) (App (tc_ref 0) [tc_ref 1]) (App (tc_ref 0) [tc_ref 2])
    -- \x y -> let z = x y in x z
tc_expr12 = Letrec [(tc_var 0, Lam [tc_var 1, tc_var 2, tc_var 3] $ Case (App (tc_ref 1) [tc_ref 3]) [(IntP 0, tc_ref 3), (WildcardP, App (tc_ref 0) [tc_ref 1, tc_ref 2, App (tc_ref 2) [tc_ref 3]])])] (tc_ref 0)
    -- letrec a = \b c d -> case b d of 0 -> d; _ -> a b c (c d) in a
tc_expr13 = Letrec [(tc_var 0, Lam [tc_var 1] (tc_ref 1))] $ App (App (tc_ref 0) [tc_ref 0]) [Lit (IntL 4)]
    -- letrec a = \b -> b in (a a) 4

test :: Expr -> IO Type
test expr = runTypecheck $ typecheckExpr M.empty expr >>= derefAll
