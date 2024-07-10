{-# LANGUAGE LambdaCase #-}
module ModalContext where
import Control.Monad.Reader
import ModalSynF
import Control.Monad (join, filterM)
import Data.Maybe (isNothing, fromJust)
import Data.Functor.Identity
import Data.Void


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
type Substitution fn var pred world elem m = 
    var -> ModalT fn var pred world elem m (Term fn var pred)

askContext :: Monad m => ModalT fn var pred world elem m (ModalContext fn var pred world elem m)
askContext = ask 
termValue :: Eq var => Eq elem => Monad m =>
    Term fn var pred -> ModalT fn var pred world elem m (Maybe elem)
termValue = \case
    Var x -> do
        context <- askContext
        lift $ varV context x
    Struct f terms -> do
        values <- mapM termValue terms
        if any isNothing values then return Nothing else do
            context <- askContext
            currentWorld <- lift $ actualWorld context
            lift $ fnV context currentWorld f (map fromJust values)
    The x formula -> do
        context <- askContext 
        currentWorld <- lift $ actualWorld context
        actualDomain <- lift $ domain context currentWorld
        elems <- query x actualDomain formula
        case elems of
            [e] -> return (Just e)
            _ -> return Nothing

query :: (Monad m, Eq elem, Eq var) => var -> [elem] -> Form fn var pred -> ModalT fn var pred world elem m [elem]
query x domain formula=
    let bindX e context = context{varV = \y -> if y==x then return (Just e) else varV context y} in
        filterM (\e -> local (bindX e) (truthValue formula)) domain

changeWorld :: Monad m =>  m world
    -> ModalT fn var pred world elem m a
    -> ModalT fn var pred world elem m a
changeWorld w = local (\context -> context{actualWorld = w})



inPossibleWorlds :: Monad m => ModalT fn var pred world elem m a
    -> ModalT fn var pred world elem m [a]
inPossibleWorlds mV = do
    context <- askContext
    w <- lift $ actualWorld context 
    worlds <- lift $ accessibility context w
    mapM (\w1 -> changeWorld (return w1)  mV) worlds


truthValue :: Eq var => Eq elem => Monad m =>
    Form fn var pred -> ModalT fn var pred world elem m Bool
truthValue = \case
    Atom p xs -> do
        context <- askContext
        currentWorld <- lift $ actualWorld context
        values <- lift $ mapM (varV context) xs
        if any isNothing values then return False
            else lift $ predV context currentWorld p (map fromJust values)
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
            then runReaderT (termValue t) context else varV context y})
        (truthValue f)

interpretFormula :: (Eq elem, Eq var) => 
    world
    -> (world -> [world])
    -> (world -> [elem])
    -> (var -> elem)
    -> (world -> fn -> [elem] -> Maybe elem)
    -> (world -> pred -> [elem] -> Bool)
    -> Form fn var pred
    -> Bool 
interpretFormula currentWorld accessibilityFn domainFn valuation fnMeaning predMeaning f =
            let context = ModalContext {
                actualWorld = return currentWorld,
                accessibility = return . accessibilityFn,
                domain = return . domainFn,
                varV = return . Just . valuation,
                fnV = \world f elems -> return (fnMeaning world f elems),
                predV = \world p elems -> return (predMeaning world p elems)
            }
            in runReader (truthValue f) context 
interpretFormulaC f = runReader (truthValue f)









