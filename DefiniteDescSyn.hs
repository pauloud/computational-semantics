module DefiniteDescSyn where
    import qualified FSynF as F
    import Data.Void ( Void )
 

    data Term var fn = Var var | Struct fn [Term var fn]
        | The var (Formula var fn) 
   

    data Formula var fn  = F (F.FormulaGen var var) 
        | Lambda var (Formula var fn) (Term var fn)
        
    
    

    


        
      

    
