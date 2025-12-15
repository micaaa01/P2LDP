module AlgoritmoW where

import AST
import Tipos
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

-- |Algoritmo W principal
-- Infiere el tipo de una expresión dado un entorno de tipos
infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env expr = case expr of
    -- Variable
    EVar x -> do
        case Map.lookup x env of
            Nothing -> error $ "Variable no ligada: " ++ x
            Just scheme -> do
                t <- instantiate scheme
                return (nullSubst, t)
    
    -- Número literal
    ENum _ ->
        return (nullSubst, TInt)
    
    -- Booleano literal
    EBool _ ->
        return (nullSubst, TBool)
    
    -- Lambda (función anónima)
    ELam x body -> do
        tv <- fresh
        let env' = Map.insert x (Forall [] tv) env
        (s1, t1) <- infer env' body
        return (s1, TFun (apply s1 tv) t1)
    
    -- Aplicación de función
    EApp e1 e2 -> do
        (s1, t1) <- infer env e1
        (s2, t2) <- infer (apply s1 env) e2
        tv <- fresh
        s3 <- unify (apply s2 t1) (TFun t2 tv)
        return (compose s3 (compose s2 s1), apply s3 tv)
    
    -- Let binding
    ELet x e1 e2 -> do
        (s1, t1) <- infer env e1
        let env' = apply s1 env
            scheme = generalize env' t1
            env'' = Map.insert x scheme env'
        (s2, t2) <- infer env'' e2
        return (compose s2 s1, t2)
    
    -- Operación binaria
    EBinOp op e1 e2 -> inferBinOp env op e1 e2
    
    -- Operación unaria
    EUnOp op e -> inferUnOp env op e

-- |Inferencia para operaciones binarias
inferBinOp :: TypeEnv -> BinOp -> Expr -> Expr -> Infer (Subst, Type)
inferBinOp env op e1 e2 = do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    
    case op of
        -- Operaciones aritméticas: Int -> Int -> Int
        Add -> do
            s3 <- unify (apply s2 t1) TInt
            s4 <- unify (apply s3 t2) TInt
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TInt)
        
        Sub -> do
            s3 <- unify (apply s2 t1) TInt
            s4 <- unify (apply s3 t2) TInt
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TInt)
        
        Mul -> do
            s3 <- unify (apply s2 t1) TInt
            s4 <- unify (apply s3 t2) TInt
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TInt)
        
        Div -> do
            s3 <- unify (apply s2 t1) TInt
            s4 <- unify (apply s3 t2) TInt
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TInt)
        
        -- Operaciones lógicas: Bool -> Bool -> Bool
        And -> do
            s3 <- unify (apply s2 t1) TBool
            s4 <- unify (apply s3 t2) TBool
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TBool)
        
        Or -> do
            s3 <- unify (apply s2 t1) TBool
            s4 <- unify (apply s3 t2) TBool
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TBool)
        
        -- Operaciones de comparación: Int -> Int -> Bool
        Equal -> do
            s3 <- unify (apply s2 t1) TInt
            s4 <- unify (apply s3 t2) TInt
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TBool)
        
        NotEqual -> do
            s3 <- unify (apply s2 t1) TInt
            s4 <- unify (apply s3 t2) TInt
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TBool)
        
        Less -> do
            s3 <- unify (apply s2 t1) TInt
            s4 <- unify (apply s3 t2) TInt
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TBool)
        
        Greater -> do
            s3 <- unify (apply s2 t1) TInt
            s4 <- unify (apply s3 t2) TInt
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TBool)
        
        LessEq -> do
            s3 <- unify (apply s2 t1) TInt
            s4 <- unify (apply s3 t2) TInt
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TBool)
        
        GreaterEq -> do
            s3 <- unify (apply s2 t1) TInt
            s4 <- unify (apply s3 t2) TInt
            let s = compose s4 (compose s3 (compose s2 s1))
            return (s, TBool)

-- |Inferencia para operaciones unarias
inferUnOp :: TypeEnv -> UnOp -> Expr -> Infer (Subst, Type)
inferUnOp env op e = do
    (s1, t1) <- infer env e
    
    case op of
        -- Negación aritmética: Int -> Int
        Neg -> do
            s2 <- unify t1 TInt
            let s = compose s2 s1
            return (s, TInt)
        
        -- Negación lógica: Bool -> Bool
        Not -> do
            s2 <- unify t1 TBool
            let s = compose s2 s1
            return (s, TBool)

-- |Función principal de inferencia de tipos
-- Devuelve el tipo inferido de una expresión
inferType :: TypeEnv -> Expr -> Type
inferType env expr =
    let (subst, t) = runInfer (infer env expr)
    in apply subst t

-- |Función auxiliar para inferir con entorno vacío
inferTypeEmpty :: Expr -> Type
inferTypeEmpty = inferType Map.empty

-- |Función para obtener el esquema de tipo más general
inferScheme :: TypeEnv -> Expr -> Scheme
inferScheme env expr =
    let t = inferType env expr
    in generalize env t

-- |Pretty printing de tipos
prettyType :: Type -> String
prettyType t = case t of
    TInt -> "Int"
    TBool -> "Bool"
    TVar v -> v
    TFun t1 t2 -> "(" ++ prettyType t1 ++ " -> " ++ prettyType t2 ++ ")"

-- |Pretty printing de esquemas
prettyScheme :: Scheme -> String
prettyScheme (Forall [] t) = prettyType t
prettyScheme (Forall vars t) =
    "forall " ++ unwords vars ++ ". " ++ prettyType t

-- |Función para mostrar el tipo inferido de forma legible
showInferredType :: Expr -> String
showInferredType expr =
    let t = inferTypeEmpty expr
    in prettyType t

-- |Función para verificar si una expresión tiene un tipo específico
hasType :: TypeEnv -> Expr -> Type -> Bool
hasType env expr expectedType =
  let inferredType = inferType env expr
  in inferredType == expectedType

