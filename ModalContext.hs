{-# LANGUAGE LambdaCase #-}
module ModalContext where
import Control.Monad.Reader
import ModalSynF
import Control.Monad (join, filterM)
import Data.Maybe (isNothing, fromJust, isJust)
import Data.Functor.Identity
import Data.Void
import Data.List (nub)


data ModalContext fn var pred world elem m = ModalContext {
    -- actualWorld and varV may be overwritten to evaluate a sub-formula
    actualWorld ::  m world, 
    accessibility :: world -> m [world],
    domain :: world -> m [elem],
    varV :: var -> m (Maybe elem), 
    fnV :: world -> fn -> [elem] ->  m (Maybe elem),
    predV :: world -> pred -> [elem] -> m Bool
}

type ModalT fn var pred world elem m =
    ReaderT (ModalContext fn var pred world elem m) m

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
        filterM (\e -> local (bindX e) (truthValue formula)) (nub domain)

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
truthValue = truthValueNormal . lambdaNormalForm where
    truthValueNormal = \case
        Atom p xs -> do
            context <- askContext
            currentWorld <- lift $ actualWorld context
            values <- mapM (termValue . Var) xs
            if any isNothing values then return False
                else lift $ predV context currentWorld p (map fromJust values)
        Eq x1 x2 -> do
            v1 <- termValue (Var x1)
            v2 <- termValue (Var x2)
            return (isJust v1 && v1 == v2)
        Neg f -> not <$> truthValueNormal f
        Impl f1 f2 -> do
            b1 <- truthValueNormal f1
            b2 <- truthValueNormal f2
            return (not b1 || b2)
        Conj fs -> and <$> mapM truthValueNormal fs
        Disj fs -> or <$> mapM truthValueNormal fs
        Lambdas [] f -> truthValueNormal f 
        Lambdas bs (Box f)  -> do
            context <- askContext
            currentWorld <- lift $ actualWorld context
            worlds <- lift $ accessibility context currentWorld
            truthValues <- mapM (\w -> changeWorld (return w) (truthValueNormal 
                (Lambdas bs f ))) worlds
            return (and truthValues)
        Lambdas bs (Diamond f)  -> truthValueNormal $ Lambdas bs (Box (Neg f)) 
        Lambdas ((x,t):bs)  f  -> do 
            maybeElem <- termValue t
            withReaderT
                (\context -> context{varV = \y -> if y == x
                then return maybeElem else varV context y})
                (truthValueNormal (Lambdas bs f))
        Box (Lambdas ((x,t):bs) f) -> do 
            maybeElem <- termValue t
            withReaderT
                (\context -> context{varV = \y -> if y == x
                then return maybeElem else varV context y})
                (truthValueNormal (Box (Lambdas bs f)))

        Box f ->  and <$> inPossibleWorlds (truthValueNormal f)
        Diamond f -> not <$> truthValue (Box f)
        _ -> error "incomplete patternmatching for truthValueNormal" 


interpretFormula :: (Eq elem, Eq var) =>
    world -- initial actual world
    -> (world -> [world]) -- accessibility relation
    -> (world -> [elem]) -- domain of each world
    -> (var -> elem) -- valuation of free vars
    -> (world -> fn -> [elem] -> Maybe elem) -- semantic of functions in each world
    -> (world -> pred -> [elem] -> Bool) -- semantic of predicates in each world
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









