module Eval1 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

------- Mónada estado-----------------------------------------------------------
newtype State a = State { runState :: Env -> (a, Env) }

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= f  = State (\s -> let (v, s') = runState m s
                            in runState (f v) s')
--------------------------------------------------------------------------------

------- Clase para representar mónadas con estado de variables -----------------
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState State where
    lookfor v = State (\s -> (lookfor' v s, s))
                where lookfor' v ((u, j):ss) | v == u = j
                                             | v /= u = lookfor' v ss
    update v i = State (\s -> ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)
--------------------------------------------------------------------------------

-- Para calmar al GHC
instance Functor State where
   fmap = liftM

instance Applicative State where
   pure   = return
   (<*>)  = ap


-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (evalComm p) initState)


-- Evalua un comando en un estado dado
evalComm :: MonadState m => Comm -> m ()
evalComm Skip              = return ()
evalComm (Let var ie)      = do ie' <- evalIntExp ie
                                update var ie'
evalComm (Seq c1 c2)       = evalComm c1 >> evalComm c2
evalComm (Cond cond cT cF) = do b <- evalBoolExp cond
                                if b then evalComm cT
                                     else evalComm cF
evalComm (While cond c)    = do b <- evalBoolExp cond
                                if b then evalComm (Seq c (While cond c))
                                     else return ()


-- Evalua una expresion entera, sin efectos laterales
evalIntExp :: MonadState m => IntExp -> m Int
evalIntExp (Const  int)    = return int
evalIntExp (Var    var)    = lookfor var
evalIntExp (UMinus ie )    = evalIntExp ie >>= \ie' -> return (-ie')
evalIntExp (Plus  ie1 ie2) = do ie1' <- evalIntExp ie1
                                ie2' <- evalIntExp ie2
                                return (ie1' + ie2')
evalIntExp (Minus ie1 ie2) = do ie1' <- evalIntExp ie1
                                ie2' <- evalIntExp ie2
                                return (ie1' - ie2')
evalIntExp (Times ie1 ie2) = do ie1' <- evalIntExp ie1
                                ie2' <- evalIntExp ie2
                                return (ie1' * ie2')
evalIntExp (Div   ie1 ie2) = do ie1' <- evalIntExp ie1
                                ie2' <- evalIntExp ie2
                                return (ie1' `div` ie2')


-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: MonadState m => BoolExp -> m Bool
evalBoolExp BFalse        = return False
evalBoolExp BTrue         = return True
evalBoolExp (Eq  ie1 ie2) = do ie1' <- evalIntExp ie1
                               ie2' <- evalIntExp ie2
                               return (ie1' == ie2')
evalBoolExp (Lt  ie1 ie2) = do ie1' <- evalIntExp ie1
                               ie2' <- evalIntExp ie2
                               return (ie1' < ie2')
evalBoolExp (Gt  ie1 ie2) = do ie1' <- evalIntExp ie1
                               ie2' <- evalIntExp ie2
                               return (ie1' > ie2')
evalBoolExp (And be1 be2) = do be1' <- evalBoolExp be1
                               be2' <- evalBoolExp be2
                               return (be1' && be2')
evalBoolExp (Or  be1 be2) = do be1' <- evalBoolExp be1
                               be2' <- evalBoolExp be2
                               return (be1' || be2')
evalBoolExp (Not be)      = evalBoolExp be >>= \be' -> return (not be')
