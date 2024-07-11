module DefiniteDescSyn where
    import qualified FSynF as F
    import Data.Void ( Void )
 

    data Term var fn = Var var | Struct fn [Term var fn]
        | The var (Formula var fn) 
   

    type Formula var fn  =  F.FormulaGen var (Term var fn)

    
        
    
    

    


        
      

    
