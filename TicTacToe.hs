module TicTacToe where
import ModalSynF

data Player = X | O deriving(Eq,Show)

type Grid = [[[Player]]]
grid1 =
    [[[O],[],[]],
     [[],[X],[]],
     [[],[X],[]]]
grid11 =
    [[[O],[],[]],
     [[O],[X],[]],
     [[],[X],[]]]
grid111 =
     [[[O],[X],[]],
     [[O],[X],[]],
     [[],[X],[]]]

grid12 =
    [[[O],[O],[]],
     [[],[X],[]],
     [[],[X],[]]]
grid121 =
    [[[O],[O],[X]],
     [[],[X],[]],
     [[],[X],[]]]

nextPlayer g
    |g == grid1 || g == grid121 = Just O
    |g == grid11 || g==grid12 = Just X
    |g == grid111 = Nothing
    |otherwise = error $ "nextPlayer function not implemented for grid " ++ show g
winner g
    |g == grid111 = Just X
    |g `elem` [grid1,grid11,grid12,grid121] = Nothing
    |otherwise = error $ "winner function not implemented for grid " ++ show g
accessibility g 
    |g == grid1 = [grid11,grid12]
    |g == grid11 = [grid111]
    |g == grid111 || g == grid121 = []
    |g == grid12 = [grid121]
    |otherwise = error $ "accessibility not implemented for grid " ++ show g 


type Domain = [(Player, Pos)]
domain :: Grid -> Domain 
domain g = [(p,(row,col))|row <- [0..2],col<-[0..2],p <- g !! row !! col]

type Pos = (Int,Int)
type Elem = (Player,Pos) 

p = Atom "P"
x = Var "x"
f x = Struct "f" (map Var x )
lastMove = Struct "lastMove" []
f1 :: Form String String String 
f1 = Lambda "x" (Lambda "x" (p ["x"]) (f ["x"])) lastMove 
    
















