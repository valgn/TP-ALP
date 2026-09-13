module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor k v = case M.lookup k v of
                Just x -> x
                Nothing -> error "No se encontró."

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update k v s = M.insert k v s

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = (Skip :!: s) 
stepComm (Let var exp) s = 
  let 
    (newVal :!: expState) = evalExp exp s
    newState = update var newVal expState
  in (Skip :!: newState)

stepComm (Seq Skip c1) s = (c1 :!: s)
stepComm (Seq c0 c1) s = 
  let
    (c0NewComm :!: c0NewState) = stepComm c0 s
  in ((Seq c0NewComm c1) :!: c0NewState)

stepComm (IfThenElse expB c0 c1) s = 
  let
    (cond :!: newState) = evalExp expB s    
  in if cond then (c0 :!: newState) else (c1 :!: newState)

stepComm (RepeatUntil c expB) s = ((Seq c (IfThenElse expB Skip (RepeatUntil c expB))) :!: s)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const x) s = (x :!: s)
evalExp (Var x) s = 
  let
    v = lookfor x s
  in (v :!: s)
evalExp (UMinus x) s = 
  let
    (v :!: ns) = evalExp x s
  in ((-v) :!: ns)  
evalExp (Plus x y) s = 
  let 
    (x' :!: nsx) = evalExp x s
    (y' :!: nsy) = evalExp y nsx
  in (x' + y' :!: nsy)
evalExp (Minus x y) s = 
  let 
    (x' :!: nsx) = evalExp x s
    (y' :!: nsy) = evalExp y nsx
  in (x' - y' :!: nsy)
evalExp (Times x y) s = 
  let 
    (x' :!: nsx) = evalExp x s
    (y' :!: nsy) = evalExp y nsx
  in (x' * y' :!: nsy)
evalExp (Div x y) s = 
  let 
    (x' :!: nsx) = evalExp x s
    (y' :!: nsy) = evalExp y nsx
  in (x' `div` y' :!: nsy)
evalExp (Eq x y) s = 
  let 
    (x' :!: nsx) = evalExp x s
    (y' :!: nsy) = evalExp y nsx
  in (x' == y' :!: nsy)
evalExp (Lt x y) s = 
  let 
    (x' :!: nsx) = evalExp x s
    (y' :!: nsy) = evalExp y nsx
  in (x' < y' :!: nsy)
evalExp (Gt x y) s = 
  let 
    (x' :!: nsx) = evalExp x s
    (y' :!: nsy) = evalExp y nsx
  in (x' > y' :!: nsy)
evalExp (NEq x y) s = 
  let 
    (x' :!: nsx) = evalExp x s
    (y' :!: nsy) = evalExp y nsx
  in (x' /= y' :!: nsy)
evalExp (And x y) s = 
  let 
    (x' :!: nsx) = evalExp x s
    (y' :!: nsy) = evalExp y nsx
  in (x' && y' :!: nsy)
evalExp (Or x y) s = 
  let 
    (x' :!: nsx) = evalExp x s
    (y' :!: nsy) = evalExp y nsx
  in ((x' || y') :!: nsy)
evalExp (Not x) s = 
  let 
    (x' :!: nsx) = evalExp x s
  in ((not x') :!: nsx)
evalExp (BTrue) s = (True :!: s)
evalExp (BFalse) s = (False :!: s)
evalExp (VarInc x) s = 
  let
    v = lookfor x s
    newState = update x (v+1) s
  in ((v+1) :!: newState)
evalExp (VarDec x) s = 
  let
    v = lookfor x s
    newState = update x (v-1) s
  in ((v-1) :!: newState)
