{-# LANGUAGE LambdaCase #-}
module FSynF where

import Data.List
import Data.List

data Column = A' | B' | C' | D' | E'
            | F' | G' | H' | I' | J'
              deriving (Eq,Ord,Show,Enum)

type Row    = Int
type Attack = (Column,Row)

data Ship = Battleship | Frigate | Submarine | Destroyer
            deriving Show

data Reaction = Missed | Hit Ship | Sunk Ship | LostBattle
                deriving Show

type Turn = (Attack,Reaction)

data Colour   = Red | Yellow | Blue | Green | Orange
                deriving (Eq,Show,Bounded,Enum)

data Answer   = Black | White deriving (Eq,Show)

type Pattern  = [Colour]
type Feedback = [Answer]

data Sent = Sent NP VP deriving Show
data NP   = SnowWhite  | Alice  | Dorothy | Goldilocks
          | LittleMook | Atreyu | Everyone | Someone
          | NP1 DET CN | NP2 DET RCN
          deriving Show
data DET  = The | Every | Some | No | Most
          deriving Show
data CN   = Girl   | Boy   | Princess | Dwarf | Giant
          | Wizard | Sword | Dagger
          deriving Show
data ADJ  = Fake deriving Show
data RCN  = RCN1 CN That VP | RCN2 CN That NP TV
          | RCN3 ADJ CN
          deriving Show
data That = That deriving Show
data VP   = Laughed | Cheered | Shuddered
          | VP1 TV NP | VP2 DV NP NP
          | VP3 AV To INF
          deriving Show
data TV   = Loved   | Admired | Helped
          | Defeated | Caught
          deriving Show

data DV   = Gave deriving Show
data AV   = Hoped | Wanted deriving Show
data INF  = Laugh | Sheer | Shudder | INF TINF NP deriving Show
data TINF = Love | Admire | Help | Defeat | Catch
            deriving Show
data To   = To deriving Show

data Form =  P String | Ng Form | Cnj [Form] | Dsj [Form]
            deriving Eq

instance Show Form where
 show (P name) = name
 show (Ng  f)  = '-': show f
 show (Cnj fs) = '&': show fs
 show (Dsj fs) = 'v': show fs

form1, form2 :: Form
form1 = Cnj [P "p", Ng (P "p")]
form2 = Dsj [P "p1", P "p2", P "p3", P "p4"]

type Name     = String
type Index    = [Int]
data Variable = Variable Name Index deriving (Eq,Ord)

instance Show Variable where
  show (Variable name [])  = name
  show (Variable name [i]) = name ++ show i
  show (Variable name is ) = name ++ showInts is
     where showInts []     = ""
           showInts [i]    = show i
           showInts (i:is) = show i ++ "_" ++ showInts is

x, y, z :: Variable
x = Variable "x" []
y = Variable "y" []
z = Variable "z" []

type Predicate = String
data FormulaGen var term = Atom Predicate [term]
               | Eq term term
               | Neg  (FormulaGen var term)
               | Impl (FormulaGen var term) (FormulaGen var term)
               | Equi (FormulaGen var term) (FormulaGen var term)
               | Conj [FormulaGen var term]
               | Disj [FormulaGen var term]
               | Forall var (FormulaGen var term)
               | Exists var (FormulaGen var term)
               | Box (FormulaGen var term)
               | Diamond (FormulaGen var term)
               deriving Eq
type Formula = FormulaGen Variable

instance Show term => Show (Formula term) where
  show (Atom s [])   = s
  show (Atom s xs)   = s ++ show xs
  show (Eq t1 t2)    = show t1 ++ "==" ++ show t2
  show (Neg form)    = '~' : show form
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>"
                           ++ show f2 ++ ")"
  show (Equi f1 f2)  = "(" ++ show f1 ++ "<=>"
                           ++ show f2 ++ ")"
  show (Conj [])     = "true"
  show (Conj fs)     = "conj" ++ show fs
  show (Disj [])     = "false"
  show (Disj fs)     = "disj" ++ show fs
  show (Forall v f)  = "A " ++  show v ++ (' ' : show f)
  show (Exists v f)  = "E " ++  show v ++ (' ' : show f)
  show (Box f) = "[]" ++ show f 
  show (Diamond f) = "<>" ++ show f


formula0 = Atom "R" [x,y] :: Formula Variable
formula1 :: FormulaGen Variable Variable
formula1 = Forall x (Atom "R" [x,x])
formula2 :: FormulaGen Variable Variable
formula2 = Forall x
            (Forall y
              (Impl (Atom "R" [x,y]) (Atom "R" [y,x])))

data TermGen var fn = Var var | Struct fn [TermGen var fn]
            deriving (Eq,Ord)
type Term = TermGen Variable String

instance Show Term where
  show (Var v)       = show v
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts

tx, ty, tz :: Term
tx = Var x
ty = Var y
tz = Var z

isVar :: TermGen var fn -> Bool
isVar (Var _) = True
isVar _       = False

varsInTerm :: Eq var => TermGen var fn -> [var]
varsInTerm (Var v)       = [v]
varsInTerm (Struct s ts) = varsInTerms ts

varsInTerms :: Eq var => [TermGen var fn] -> [var]
varsInTerms = nub . concatMap varsInTerm



freeVars :: Eq var => FormulaGen var (TermGen var fn) -> [var]
freeVars = nub . freeVarsR where
  freeVarsR = \case
    Atom _ ts -> varsInTerms ts
    Eq t1 t2 -> varsInTerm t1 ++ varsInTerm t2
    Impl f1 f2 -> freeVarsR f1 ++ freeVarsR f2
    Conj fs -> concatMap freeVarsR fs
    Disj fs -> concatMap freeVarsR fs
    Forall x f -> [y | y <- freeVarsR f, y /= x ]
    Exists x f -> [y | y <- freeVarsR f, y /= x ]
    Box f -> freeVarsR f
    Diamond f -> freeVarsR f


hasFreeVarsExcept :: Eq var => [var] -> FormulaGen var (TermGen var fn) -> Bool
hasFreeVarsExcept xs f = [y | y<-freeVars f, y `notElem` xs] /= []

isModal :: FormulaGen var term -> Bool
isModal = \case 
  Atom _ _ -> False 
  Eq _ _ -> False 
  Box _ -> True 
  Diamond _ -> True
  Impl f1 f2 -> isModal f1 && isModal f2 
  Conj fs -> any isModal fs 
  Disj fs -> any isModal fs 
  Forall _ f -> isModal f 
  Exists _ f -> isModal f 
   
  

