{-# LANGUAGE RecordWildCards #-}
module ModalSem where
    import ModalSyn
    import Data.Maybe(isJust,fromJust)
    import qualified FSynF as F
    import Data.Set


    data World a = World {
        domain :: Set a,
        pred :: String -> [a] -> Bool,
        func :: String -> [a] -> Maybe a}
    data Model a = Model (World a) [World a]

    truthValue :: (F.Variable -> Maybe a) -> Model a -> Formula -> Bool 
    truthValue freeVarsBinding model modalFormulaWithDefiniteDescription = undefined 



