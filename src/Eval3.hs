module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple


-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacíoz
-- Completar la definición
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor k (m, _) = case M.lookup k m of
                Just x -> Right x
                Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update k v (m, tr) = (M.insert k v m, tr)

-- Evalúa un programa en el estado vacío
-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = Right s
stepCommStar c s = 
  case stepComm c s of
    Right (c' :!: s') -> stepCommStar c' s' 
    Left e -> Left e

-- Evalúa un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip s = Right (Skip :!: s)

stepComm (Let var exp) s = 
  case evalExp exp s of 
    Right (newVal :!: expState) -> 
      let 
        (newMap, tr) = update var newVal expState 
        currentCmd = "Let " ++ var ++ " " ++ show newVal
        newTrace = if tr == "" then currentCmd else tr ++ " " ++ currentCmd
      in Right (Skip :!: (newMap, newTrace))
    Left e -> Left e

stepComm (Seq Skip c1) s = (Right (c1 :!: s))

stepComm (Seq c0 c1) s = 
  case stepComm c0 s of
    Right (c0NewComm :!: c0NewState) -> Right (Seq c0NewComm c1 :!: c0NewState)
    Left e -> Left e

stepComm (IfThenElse expB c0 c1) s = 
  case evalExp expB s of 
    Right (cond :!: newState) -> if cond then Right (c0 :!: newState) else Right (c1 :!: newState)
    Left e -> Left e

stepComm (RepeatUntil c expB) s = 
  Right (Seq c (IfThenElse expB Skip (RepeatUntil c expB)) :!: s)
 

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const x) s = Right (x :!: s)
evalExp (Var x) s = 
  case lookfor x s of
    Right val -> Right (val :!: s)
    Left e  -> Left e
    
evalExp (UMinus x) s = 
  case evalExp x s of
    Right (v :!: ns) -> Right ((-v) :!: ns) 
    Left e -> Left e   

evalExp (Plus x y) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> case evalExp y nsx of 
                                Right (y' :!: nsy) -> Right (x' + y' :!: nsy)
                                Left e -> Left e
    Left e -> Left e

evalExp (Minus x y) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> case evalExp y nsx of 
                                Right (y' :!: nsy) -> Right (x' - y' :!: nsy)
                                Left e -> Left e
    Left e -> Left e

evalExp (Times x y) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> case evalExp y nsx of 
                                Right (y' :!: nsy) -> Right (x' * y' :!: nsy)
                                Left e -> Left e
    Left e -> Left e

evalExp (Div x y) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> case evalExp y nsx of 
                                Right (y' :!: nsy) -> if y' == 0 then (Left DivByZero) else Right (x' `div` y' :!: nsy)
                                Left e -> Left e
    Left e -> Left e
    
evalExp (Eq x y) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> case evalExp y nsx of 
                                Right (y' :!: nsy) -> Right (x' == y' :!: nsy)
                                Left e -> Left e
    Left e -> Left e

evalExp (Lt x y) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> case evalExp y nsx of 
                                Right (y' :!: nsy) -> Right (x' < y' :!: nsy)
                                Left e -> Left e
    Left e -> Left e

evalExp (Gt x y) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> case evalExp y nsx of 
                                Right (y' :!: nsy) -> Right (x' > y' :!: nsy)
                                Left e -> Left e
    Left e -> Left e

evalExp (NEq x y) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> case evalExp y nsx of 
                                Right (y' :!: nsy) -> Right (x' /= y' :!: nsy)
                                Left e -> Left e
    Left e -> Left e
    
evalExp (And x y) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> case evalExp y nsx of 
                                Right (y' :!: nsy) -> Right ((x' && y') :!: nsy)
                                Left e -> Left e
    Left e -> Left e

evalExp (Or x y) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> case evalExp y nsx of 
                                Right (y' :!: nsy) -> Right ((x' || y') :!: nsy)
                                Left e -> Left e
    Left e -> Left e

evalExp (Not x) s = 
  case evalExp x s of
    Right (x' :!: nsx) -> Right ((not x') :!: nsx)
    Left e -> Left e
  
evalExp (BTrue) s = Right (True :!: s)

evalExp (BFalse) s = Right (False :!: s)

evalExp (VarInc x) s = 
  case lookfor x s of 
    Right v -> let newState = update x (v+1) s in Right ((v+1) :!: newState)
    Left e -> Left e 
  
  
evalExp (VarDec x) s = 
  case lookfor x s of 
    Right v -> let newState = update x (v-1) s in Right ((v-1) :!: newState)
    Left e -> Left e 
