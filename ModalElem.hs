{-# LANGUAGE LambdaCase #-}
module ModalElem where
import Control.Monad.Reader
import ModalSynF
import Control.Monad (join)
import Data.Maybe (isNothing, fromJust)


data ModalContext fn var pred world elem m = ModalContext {
    actualWorld ::  ModalT fn var pred world elem m world,
    accessibility :: world -> ModalT fn var pred world elem m [world],
    domain :: world -> ModalT fn var pred world elem m [elem],
    fnV :: fn -> [elem] -> ModalT fn var pred world elem m (Maybe elem),
    varV :: var -> ModalT fn var pred world elem m (Maybe elem),
    predV :: pred -> [elem] -> ModalT fn var pred world elem m Bool
}


type ModalT fn var pred world elem m =
    ReaderT (ModalContext fn var pred world elem m) m

termValue :: Monad m => Term fn var pred -> ModalT fn var pred world elem m (Maybe elem)
termValue = \case
    Var x -> do
        context <- ask
        varV context x
    Struct f terms -> do
        values <- mapM termValue terms
        if any isNothing values then return Nothing else do
            context <- ask
            fnV context f (map fromJust values)
    The x formula -> return $ error "definite description not yet implemented"


changeWorld :: Monad m =>  ModalT fn var pred world elem m world
    -> ModalT fn var pred world elem m a
    -> ModalT fn var pred world elem m a
changeWorld w = local (\context -> context{actualWorld = w})

inPossibleWorlds :: Monad m => ModalT fn var pred world elem m a
    -> ModalT fn var pred world elem m [a]
inPossibleWorlds mV = do
    context <- ask
    worlds <- actualWorld context >>= accessibility context
    mapM (\w -> changeWorld (return w)  mV) worlds

type Form fn var pred = Formula (Term fn var pred) var pred
truthValue :: Eq var => Eq elem => Monad m => 
    Form fn var pred -> ModalT fn var pred world elem m Bool
truthValue = \case
    Atom p xs -> do
        context <- ask
        values <- mapM (varV context) xs
        if any isNothing values then return False 
            else predV context p (map fromJust values)
    Eq t1 t2 -> do
        v1 <- termValue t1
        v2 <- termValue t2
        return (v1 == v2)
    Neg f -> not <$> truthValue f
    Impl f1 f2 -> do 
       b1 <- truthValue f1 
       b2 <- truthValue f2 
       return (not b1 || b2)
    Conj fs -> and <$> mapM truthValue fs 
    Disj fs -> or <$> mapM truthValue fs 
    Box f -> do
        truthValues <- inPossibleWorlds (truthValue f)
        return (and truthValues)
    Diamond f -> do
        truthValues <- inPossibleWorlds (truthValue f)
        return (or truthValues)
    Lambda x f t -> withReaderT
        (\context -> context{varV = \y -> if y == x 
            then termValue t else varV context y})
        (truthValue f)
    
   






