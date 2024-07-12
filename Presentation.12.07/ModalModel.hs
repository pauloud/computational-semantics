{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module ModalModel where
    import ModalContext
    import Control.Monad.Identity
    import ModalSynF 


    data World = W0 | W1 | W2 deriving (Eq,Show)
    data Elem = E0 | E1  deriving (Eq,Show)
    data Fn = A | F deriving (Eq,Show)
    data Pred = P deriving(Eq,Show)



    graph0 = \case 
        W0 -> [W1,W2]
        _ ->  []

    domain0 = const [E0,E1]

    fn0 W0 A _ = return $ Just E0
    fn0 W1 A _ = return $ Just E1
    fn0 W2 A _ = return $ Just E1

    pred0 _ P [E0] = return True 
    pred0 _ P [E1] = return False 

    noFreeVar :: String -> m (Maybe elem)
    noFreeVar x = error $ "no valuation for variable " ++ x

    context0 :: ModalContext Fn String Pred World Elem Identity 
    context0 = ModalContext (return W0) (return.graph0) (return.domain0)  fn0 noFreeVar pred0 




    p = Atom P
    x = Var "x"
    a = Struct A []
    f0 :: Form Fn String Pred
    f0 = Box (Lambda "x" (p ["x"]) a)
    f1 :: Form Fn String Pred
    f1 = Lambda "x" (Box (p["x"])) a