module Eval3 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

------- Mónada estado-----------------------------------------------------------
newtype StateErrorTick a =
    StateErrorTick { runStateErrorTick :: Env -> Maybe (a, Env, Int) }

instance Monad StateErrorTick where
    return x = StateErrorTick $ \s -> Just (x, s, 0)
    m >>= f  = StateErrorTick $ \s -> do (v, s', c)    <- (runStateErrorTick m) s
                                         (v', s'', c') <- (runStateErrorTick (f v)) s'
                                         return (v', s'', c+c')
--------------------------------------------------------------------------------

------- Clase para representar mónadas con estado de variables -----------------
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState StateErrorTick where
    lookfor v = StateErrorTick $ \s -> case lookfor' v s of
                    Just m  -> Just (m, s, 0)
                    Nothing -> Nothing
                where lookfor' v [] = Nothing
                      lookfor' v ((u, j):ss) | v == u = Just j
                                             | v /= u = lookfor' v ss
    update v i = StateErrorTick $ \s -> Just ((), update' v i s, 0)
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)
--------------------------------------------------------------------------------

------- Clase para representar mónadas que lanzan errores ----------------------
class Monad m => MonadError m where
   -- Lanza un error
   throw :: m a

instance MonadError StateErrorTick where
    throw = StateErrorTick (\_ -> Nothing)
--------------------------------------------------------------------------------

------- Clase para representar mónadas con conteo de operaciones aritméticas ---
class Monad m => MonadTick m where
    -- Incrementa el contador en una unidad
    tick :: m ()

instance MonadTick StateErrorTick where
    tick = StateErrorTick $ \s -> Just ((), s, 1)
--------------------------------------------------------------------------------

-- Para calmar al GHC
instance Functor StateErrorTick where
    fmap = liftM

instance Applicative StateErrorTick where
    pure   = return
    (<*>)  = ap


-- Evalua un programa en el estado nulo
eval :: Comm -> (Env, Int)
eval p = case runStateErrorTick (evalComm p) initState of
            Just (v, s, c) -> (s, c)
            Nothing        -> error "Ocurrió un error!"


-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m, MonadTick m) => Comm -> m ()
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
evalIntExp :: (MonadState m, MonadError m, MonadTick m) => IntExp -> m Int
evalIntExp (Const  int)    = return int
evalIntExp (Var    var)    = lookfor var
evalIntExp (UMinus ie )    = evalIntExp ie >>= \ie' -> return (-ie')
evalIntExp (Plus  ie1 ie2) = do ie1' <- evalIntExp ie1
                                ie2' <- evalIntExp ie2
                                tick
                                return (ie1' + ie2')
evalIntExp (Minus ie1 ie2) = do ie1' <- evalIntExp ie1
                                ie2' <- evalIntExp ie2
                                tick
                                return (ie1' - ie2')
evalIntExp (Times ie1 ie2) = do ie1' <- evalIntExp ie1
                                ie2' <- evalIntExp ie2
                                tick
                                return (ie1' * ie2')
evalIntExp (Div   ie1 ie2) = do ie1' <- evalIntExp ie1
                                ie2' <- evalIntExp ie2
                                if ie2' == 0
                                    then throw
                                    else do tick
                                            return (ie1' `div` ie2')


-- Evalua una expresion entera, sin efectos laterales
evalBoolExp :: (MonadState m, MonadError m, MonadTick m) => BoolExp -> m Bool
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
