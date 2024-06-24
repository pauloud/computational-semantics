{-# LANGUAGE RecordWildCards,LambdaCase #-}

module ModalSem where
    import ModalSyn
    import Data.Maybe(isJust,fromJust)
    import qualified FSynF as F
    import qualified Data.Set as Set


    data World a = World {
        domain :: Set.Set a,
        pred :: String -> [a] -> Bool,
        func :: String -> [a] -> Maybe a}
    data Model a = Model (World a) [World a]

    bindVar :: F.Variable -> a -> (F.Variable -> Maybe a) -> F.Variable -> Maybe a
    bindVar v e func v1 = if v1 == v then Just e else func v1

    allIn :: Ord a => [a] -> Set.Set a -> Bool
    allIn list set = all (`Set.member` set) list 

   
        
    termValues :: Ord a => [Term a] -> (F.Variable -> Maybe a) -> Model a -> [Maybe a]
    termValues terms binding model@(Model world possibleWorlds) = map termValue terms 
        where termValue = \case 
                Elem e -> if Set.member e (domain world) then Just e else Nothing
                T (F.Var x) -> binding x 
                T (F.Struct f t) -> if all isJust values then func world f $ map fromJust values else Nothing
                    where values = termValues (map T t) binding model 
    

    
{-
    truthValue :: (F.Variable -> Maybe a) -> Model a -> PropositionBuilder a -> Bool 
    truthValue binding (Model World{..} possibleWorlds) = \case 
        F (Atom p terms) ->
    
    -}