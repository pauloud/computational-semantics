{-# LANGUAGE LambdaCase #-}
module WithToken where
    import Data.Tree

    data WithAST tok a where 
        Leaf :: tok -> WithAST tok a 
        App :: WithAST tok (a -> b) -> WithAST tok a -> WithAST tok b 
        Lambda :: tok -> WithAST tok (a -> b) -> WithAST tok a -> WithAST tok b 

    getAST :: WithAST tok a -> Tree tok 
    getAST = \case 
        Leaf t -> Node t []
        App 
        
    


  