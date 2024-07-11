
{-# LANGUAGE LambdaCase,GADTs #-}
module ModalSynF where
import Data.Maybe (isJust)
import Data.List (deleteBy)

data Term fn var pred = Var var | Struct fn [Term fn var pred]
        | The var (Formula (Term fn var pred) var pred)
-- definite description is 
type Form fn var pred = Formula (Term fn var pred) var pred
data Formula term var pred = Atom pred [var]
               | Eq var var
               | Neg  (Formula term var pred)
               | Impl (Formula term var pred) (Formula term var pred)
               | Equi (Formula term var pred) (Formula term var pred)
               | Conj [Formula term var pred]
               | Disj [Formula term var pred]
               | Forall var (Formula term var pred)
               | Exists var (Formula term var pred)
               | Box (Formula term var pred)
               | Diamond (Formula term var pred)
               |Lambda var (Formula term var pred) term
               |Lambdas [(var,term)] (Formula term var pred)
               deriving Eq

 

-- quantifiers then lambdas de dicto then modalities
lambdaNormalForm f = case f of
        Atom _ _ -> f
        Eq _ _ -> f
        Neg (Box f1) -> Diamond (Neg (lambdaNormalForm f1))
        Neg (Diamond f1) -> Box (Neg (lambdaNormalForm f1))
        Neg (Neg f1) -> lambdaNormalForm f1
        Neg f1 -> Neg (lambdaNormalForm f1)
        Impl f1 f2 -> Impl (lambdaNormalForm f1 ) (lambdaNormalForm f2)
        Equi f1 f2 -> Equi (lambdaNormalForm f1 ) (lambdaNormalForm f2)
        Conj fs ->Conj (map lambdaNormalForm fs)
        Disj fs -> Disj (map lambdaNormalForm fs)
        Forall x f1 -> Forall x (lambdaNormalForm f1)
        Exists x f1 -> Exists x (lambdaNormalForm f1)
        Box f1 -> Box (lambdaNormalForm f1)
        Diamond f1 -> Diamond (lambdaNormalForm f1)
        Lambda x f1 t -> lambdaNormalForm (Lambdas [(x,t)] f1)
        Lambdas [] f1 -> lambdaNormalForm f1 --optionnal 
        Lambdas bs (Forall x f1)  -> if isJust (lookup x bs)
                then let bs' = filter (\(y,_) -> y == x) bs in lambdaNormalForm (Lambdas bs' f)
                else Forall x (lambdaNormalForm (Lambdas bs f1 ))
        Lambdas bs (Exists x f1)  -> if isJust (lookup x bs)
                then let bs' = filter (\(y,_) -> y == x) bs in lambdaNormalForm (Lambdas bs' f)
                else Exists x (lambdaNormalForm (Lambdas bs f1 ))
        Lambdas bs (Impl f1 f2)  -> Impl (Lambdas bs f1 ) (Lambdas bs f2)
        Lambdas bs (Equi f1 f2)  -> Equi (Lambdas bs f1 ) (Lambdas bs f2 )
        Lambdas bs (Conj fs)  -> Conj $ map (Lambdas bs . lambdaNormalForm ) fs
        Lambdas bs (Disj fs) -> Disj $ map (Lambdas bs . lambdaNormalForm ) fs
        Lambdas bs (Lambda x f1 t)  -> lambdaNormalForm (Lambdas (bs ++ [(x,t)]) f1)
        Lambdas bs f1 -> Lambdas bs (lambdaNormalForm f1)

{-             
applySubstitution :: (t -> Term fn var pred1) -> Term fn t pred2 -> Term fn var pred1
applySubstitution s = \case
        Var x -> s x
        Struct fn ts -> Struct fn (map (applySubstitution s) ts)
        The x f -> error "substitution not yet implemented for definite description"
substitute x t s y = if y==x then applySubstitution s t else s y
-}











