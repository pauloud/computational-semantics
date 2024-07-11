module WithToken where
    import Data.Tree

    data WithAST tok a = WithAST (Tree tok) a