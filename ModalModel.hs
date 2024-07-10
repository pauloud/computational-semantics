{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module ModalModel where
    import ModalContext
    import Control.Monad.Identity
    
    context :: ModalContext String String String Int [Int] Identity 
    context = ModalContext {
        actualWorld = return 0 
        ,accessibility = \case 
            0 -> return [1,2]
        ,domain = \case 
            0 -> return [0,1,2]
            1 -> return [0,1,2]
            2 -> return [1,2]
        ,predV = \_ p xs -> if p == "P" then return (elem xs 0) else error $ "predicate " ++ p ++ " uninterpreted"
        ,fnV = \_ f x -> if f == "f" then if x <= 0 then return Nothing else return (x-1)
            else error $ "function " ++ f ++ " uninterpreted"
        ,varV = \x ->  error $ "variable " ++ x ++ " uninterpreted"
    }
    p = Atom "P"
    x = Var "x"
    f x = Struct "f" (map Var x )
    a = Struct "a" []
    f1 :: Form String String String 
    f1 = Lambda "x" (Lambda "x" (p ["x"]) (f ["x"])) a 

    

