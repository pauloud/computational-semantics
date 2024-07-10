{-# LANGUAGE LambdaCase #-}

module ModalSem where
    import Data.Maybe(isJust,fromJust)
    import Data.Foldable(find)
    import ModalSynF
    import qualified Data.Set as Set
    import Prelude hiding (pred)

     

    data World fn pred a = World {
        existsIn :: a -> Bool,
        rel :: pred -> [a] -> Bool,
        func :: fn  -> Maybe ([a] -> Maybe a)}
    data Graph pred fn a = Graph {world :: World fn pred a,
        neighbors :: [Graph pred fn a]}

   

    data PModel fn pred a = PModel {
        wholeDomain :: Set.Set a,
        graph :: Graph pred fn a}

    bindVar :: Eq var => var  -> Maybe a -> (var -> Maybe a) -> var -> Maybe a
    bindVar v e func v1 = if v1 == v then e else func v1

    allIn :: Ord a => [a] -> Set.Set a -> Bool
    allIn list set = all (`Set.member` set) list


    query :: (Ord a,Eq var) => var  -> (var -> Maybe a) -> PModel fn pred a -> Form fn var pred -> [a]
    --query x binding model formula = filter (\e -> truthValue (bindVar x (Just e) binding) model formula) (Set.toList $ wholeDomain model)
    query x binding model formula =
        [e | e <- Set.toList (wholeDomain model),truthValue (bindVar x (Just e) binding) model formula]
--f(t,u) 
    termValue :: (Ord a,Eq var) => (var -> Maybe a) -> PModel fn pred a -> Term fn var pred -> Maybe a
    termValue binding model@(PModel _ (Graph world _))  = \case
                Var x ->find (existsIn world) (binding x)
                Struct f ts -> if all isJust values then func world f >>= (\f -> f $ map fromJust values) else Nothing
                    where values = termValues binding model ts
                The var formula -> case query var binding model formula of
                    [e] -> Just e
                    _ -> Nothing
    termValues :: (Ord a,Eq var) => (var -> Maybe a) -> PModel fn pred a -> [Term fn var pred] -> [Maybe a]
    termValues binding model = map (termValue binding model)


    -- Exists and Forall should quantify only on actual world
    -- Semantic of Lambda abstraction is not sound
    truthValue :: (Ord a, Eq var) => (var -> Maybe a) -> PModel fn pred a -> Form fn var pred -> Bool
    truthValue binding model@(PModel wholeDomain (Graph world possibleWorlds)) = \case
        Atom p vars -> all (isJust.binding) vars && rel world p (map (fromJust.binding) vars)
        Eq x y -> isJust (termValue binding model x) && (termValue binding model x == termValue binding model y) -- && existsIn world (fromJust $ binding x)
        Neg f -> not $ truthValue binding model f
        Impl f1 f2 ->  not (truthValue binding model f1) || truthValue binding model f2
        Equi f1 f2 -> truthValue binding model f1 == truthValue binding model f2
        Conj fs -> all (truthValue binding model) fs
        Disj fs -> any (truthValue binding model) fs
        Forall x f -> all (\e -> truthValue (bindVar x (Just e) binding) model f) wholeDomain
        Exists x f -> any (\e -> truthValue (bindVar x (Just e) binding) model f) wholeDomain
        Box f -> all (\w -> truthValue binding (PModel wholeDomain w) f) possibleWorlds
        Diamond f -> any (\w -> truthValue binding (PModel wholeDomain w) f) possibleWorlds
        Lambda x f t -> let b = bindVar x (termValue binding model t) binding
                in truthValue b model f
         






