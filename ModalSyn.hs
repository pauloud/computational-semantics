module ModalSyn where
    import qualified FSynF as F
    import Data.Void ( Void )
    type Function = String 

    data Term var fn = Var var | Struct fn [Term var fn] 
        |The var (Formula var fn) deriving(Show,Eq)
    data Formula var fn  = F (F.Formula var) | Lambda var (Formula var fn) (Term var fn)
        |Box (Formula var fn) deriving (Show,Eq)
      

    
