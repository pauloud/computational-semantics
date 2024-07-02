{-# LANGUAGE LambdaCase,PatternSynonyms #-}
module DefiniteDefSyn where
    import qualified FSynF as F
    import Data.Void ( Void )
 

    data Term var fn = T (F.TermGen var fn)
        | The var (Formula var fn) 
    pattern Var x = T (F.Var x)
    pattern Struct f ts = T (F.Struct f ts)

    data Formula var fn  = F (F.FormulaGen var var) 
        | Lambda var (Formula var fn) (Term var fn)
        
    
    

    


        
      

    
