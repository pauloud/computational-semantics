module ModalSyn where
    import qualified FSynF as F
    import Data.Void ( Void )
    type Function = String 
    data Term a = T F.Term | The F.Variable Formula |Elem a deriving (Show,Eq)
    data PropositionBuilder a = F (F.Formula a) | Lambda F.Variable Formula (Term a)
        |Box Formula deriving (Show,Eq)
    type Formula = PropositionBuilder Void  

    
