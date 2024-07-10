{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}
module ModalSynF where

data Term fn var pred = Var var | Struct fn [Term fn var pred]
        | The var (Formula (Term fn var pred) var pred) 

type Form fn var pred = Formula (Term fn var pred) var pred
data Formula term var pred = Atom pred [var]
               | Eq term term
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
               deriving Eq

toLambdaHead f = case f of 
        Lambda x f1 t -> case f1 of 
                Impl f2 f3 -> Impl (Lambda x (toLambdaHead f2) t) (Lambda x (toLambdaHead f3) t)
                Equi f2 f3 -> Equi (Lambda x (toLambdaHead f2) t) (Lambda x (toLambdaHead f3) t)
                Conj fs -> Conj $ map (\f2 -> Lambda x (toLambdaHead f2) t) fs 
                Disj fs -> Disj $ map (\f2 -> Lambda x (toLambdaHead f2) t) fs 
                Forall x1 f2 -> Forall x1 (toLambdaHead (Lambda x f2 t)) 
                Exists x1 f2 -> Exists x1 (toLambdaHead (Lambda x f2 t)) 
                Box f2 -> Lambda x (Box (toLambdaHead f2)) t
                Diamond f2 -> Lambda x (Diamond (toLambdaHead f2)) t

                

