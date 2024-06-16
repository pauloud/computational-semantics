module ModalSyn where
    import qualified FSynF as F

    type Function = String 
    data Term = T F.Term | The F.Variable Formula deriving (Show,Eq)
    data Formula = F (F.Formula F.Variable) | Lambda F.Variable Formula Term 
        |Box Formula deriving (Show,Eq)

    
