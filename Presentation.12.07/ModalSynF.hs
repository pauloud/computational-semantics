{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module ModalSynF where

data Term fn var pred = Var var | Struct fn [Term fn var pred]
        | The var (Formula (Term fn var pred) var pred) 
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
