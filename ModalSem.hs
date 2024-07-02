{-# LANGUAGE LambdaCase #-}

module ModalSem where
    import DefiniteDefSyn
    import Data.Maybe(isJust,fromJust)
    import qualified FSynF as F
    import qualified Data.Set as Set
    import Prelude hiding (pred)


    data World fn a = World {
        domain :: Set.Set a,
        pred :: String -> [a] -> Bool,
        func :: fn  -> Maybe ([a] -> Maybe a)}
    data Graph fn a = Graph {world :: World fn a,
        neighbors :: [Graph fn a]}

    possibleWorlds :: Graph fn a -> [World fn a]
    possibleWorlds = map world . neighbors 
    
    data PModel fn a = PModel {
        wholeDomain :: Set.Set a,
        graph :: Graph fn a}

    bindVar :: Eq var => var  -> Maybe a -> (var -> Maybe a) -> var -> Maybe a
    bindVar v e func v1 = if v1 == v then e else func v1

    allIn :: Ord a => [a] -> Set.Set a -> Bool
    allIn list set = all (`Set.member` set) list


    query :: (Ord a,Eq var) => var  -> (var -> Maybe a) -> PModel fn a -> Formula var fn -> [a]
    --query x binding model formula = filter (\e -> truthValue (bindVar x (Just e) binding) model formula) (Set.toList $ wholeDomain model)
    query x binding model formula =
        [e | e <- Set.toList (wholeDomain model),truthValue (bindVar x (Just e) binding) model formula]
--f(t,u) 
    termValue :: (Ord a,Eq var) => (var -> Maybe a) -> PModel fn a -> Term var fn -> Maybe a
    termValue binding model@(PModel _ (Graph world _))  = \case
                Var x -> binding x
                Struct f ts -> if all isJust values then func world f >>= (\f -> f $ map fromJust values) else Nothing
                    where values = termValues binding model ts
                The var formula -> case query var binding model formula of
                    [e] -> Just e
                    _ -> Nothing 
    termValues :: (Ord a,Eq var) => (var -> Maybe a) -> PModel fn a -> [Term var fn] -> [Maybe a]
    termValues binding model = map (termValue binding model)



    truthValue :: (Ord a, Eq var) => (var -> Maybe a) -> PModel fn a -> Formula var fn -> Bool
    truthValue binding model@(PModel _ (Graph world _)) =
        let atomTruth p vars = (all (isJust . binding) vars && pred world p (map (fromJust.binding) vars))
            truthV = \case
                F.Atom p vars -> atomTruth p vars
                F.Eq x y -> ((isJust (binding x) && isJust (binding y)) && (fromJust (binding x) == fromJust (binding y)))
                F.Neg f -> not $ truthV f
                F.Impl f1 f2 ->  not (truthV f1) || truthV f2
                F.Equi f1 f2 -> truthV f1 == truthV f2
                F.Conj fs -> all truthV fs
                F.Disj fs -> any truthV fs
                F.Forall x f -> all (\e -> truthValue (bindVar x (Just e) binding) model $ F f) $ domain world 
                F.Exists x f -> any (\e -> truthValue (bindVar x (Just e) binding) model $ F f) $ domain world
                _ -> error "modalities not yet defined"
            in \case
            F f -> truthV f
            Lambda x f t -> let b = bindVar x (termValue binding model t) binding
                in truthValue b model f
          





