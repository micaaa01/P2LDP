module Tipos where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

-- Variables de tipo
type TVar = String 

-- Tipos
data Type
  = TInt
  | TBool
  | TVar TVar
  | TFun Type Type
  deriving (Eq, Show)

-- Esquemas
data Scheme = Forall [TVar] Type
  deriving (Eq, Show)

-- Esquemas, sustituciones e inferencia
type TypeEnv = Map.Map String Scheme
type Subst = Map.Map TVar Type
type Infer a = State Int a

nullSubst :: Subst
nullSubst = Map.empty

-- Sustituciones
class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar


-- Instancias
instance Substitutable Type where
  apply s t = case t of
    TInt       -> TInt
    TBool      -> TBool
    TVar a     -> Map.findWithDefault (TVar a) a s
    TFun t1 t2 -> TFun (apply s t1) (apply s t2)

  -- variables libres
  ftv t = case t of
    TInt       -> Set.empty
    TBool      -> Set.empty
    TVar a     -> Set.singleton a
    TFun t1 t2 -> ftv t1 `Set.union` ftv t2

-- Manejo del caso de sustitución para variables cuantificadas (alcance lexico logico)
instance Substitutable Scheme where
  apply s (Forall vars t) =
    Forall vars (apply s' t)
    where
      s' = foldr Map.delete s vars

  ftv (Forall vars t) =
    ftv t `Set.difference` Set.fromList vars

-- Actualiza el entorno 
instance Substitutable TypeEnv where
  apply s = Map.map (apply s)
  ftv env = Set.unions (map ftv (Map.elems env))

--inferencia
-- genera variables frescas
runInfer :: Infer a -> a
runInfer m = evalState m 0

fresh :: Infer Type
fresh = do
  i <- get
  put (i + 1)
  return (TVar ("a" ++ show i))

-- Composicion de sustituciones
compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2  `Map.union` s1

unify :: Type -> Type -> Infer Subst
unify t1 t2 = case (t1, t2) of
        -- Constantes iguales
        (TInt, TInt) -> 
            return nullSubst
        (TBool, TBool) ->
            return nullSubst
        -- Variables con tipo
        (TVar a, t) ->
            bind a t
        (t, TVar a) ->
            bind a t
        -- Funciones
        (TFun l1 r1, TFun l2 r2) -> do
            s1 <- unify l1 l2
            s2 <- unify (apply s1 r1) (apply s1 r2)
            return (compose s2 s1)
        -- Error
        _ ->
            error $ "No se puede unificar: " ++ show t1 ++ " con " ++ show t2

--Evita ciclos
bind :: TVar -> Type -> Infer Subst
bind a t
    | t == TVar a            = return nullSubst
    | a `Set.member` ftv t   = error $ "Choque: " ++ a ++ " en " ++ show t
    | otherwise              = return (Map.singleton a t)

