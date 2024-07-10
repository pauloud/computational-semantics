{-# LANGUAGE LambdaCase,GADTs #-}
module ModalSynG where 
import Control.Monad.Reader
import Data.Maybe

data FromWorld a 
data ModalElem term var pred a where
    
    Atom :: pred -> [var] -> ModalElem term var pred Bool
    Op :: ([a] -> b)-> [ModalElem term var pred a] -> ModalElem term var pred b 
    Box :: ModalElem term var pred a -> ModalElem term var pred (FromWorld a)
    Unmodal :: ModalElem term var pred (FromWorld a) -> ModalElem term var pred a 
    Forall :: var -> ModalElem term var pred Bool -> ModalElem term var pred Bool
    Lambda :: var -> ModalElem term var pred a-> term -> ModalElem term var pred a


    
data ModalContext fn var pred world elem m = ModalContext {
    actualWorld ::  m world,
    accessibility :: world -> m [world],
    domain :: world -> m [elem],
    varV :: var -> m (Maybe elem),
    fnV :: world -> fn -> [elem] ->  m (Maybe elem),
    predV :: world -> pred -> [elem] -> m Bool
}


type ModalT fn var pred world elem m =
    ReaderT (ModalContext fn var pred world elem m) m
type Form term var pred = ModalElem term var pred Bool 
truthValue :: Eq var => Eq elem => Monad m =>
    Form fn var pred -> ModalT fn var pred world elem m Bool
askContext :: Monad m => ModalT fn var pred world elem m (ModalContext fn var pred world elem m)
askContext = ask 
truthValue = \case 
    Atom p xs -> do 
        context <- askContext
        currentWorld <- lift $ actualWorld context
        values <- mapM termValue xs
        if any isNothing values then return False
            else lift $ predV context currentWorld p (map fromJust values)



  