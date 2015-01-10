module Eval where
--import IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import Lexchi
import Parchi
import Skelchi
import Printchi
import Abschi
import ErrM

substs :: Exp -> [LIdent] -> [Exp] -> Exp
substs exp1 (i:[]) (exp2:[])   = subst exp1 i exp2
substs exp1 (i:is) (exp2:exps) = substs (subst exp1 i exp2) is exps
substs _ is es                 = error "Invalid constructor"

subst :: Exp -> LIdent -> Exp -> Exp
subst (Apply e' es) i e   = Apply (subst e' i e) (map (\x -> subst x i e) es)
subst (Lambda is e') i e  | i `elem` is = Lambda is e'
                          | otherwise   = Lambda  is (subst e' i e)   
subst (Case e' bs) i e    = Case (subst e' i e) (map (\(Branch ui exp) -> (Branch ui (subst exp i e))) bs)
subst (Rec j e') i e      | j == i    = Rec j e'
                          | otherwise = Rec j (subst e' i e)
subst (Var j) i e         | j == i    = e
                          | otherwise = Var j
subst (Const ui) i e      = Const ui
subst _ i e               = error "Invalid constructor"

  

lookupBr :: UIdent -> [Branch] -> Maybe Exp
lookupBr ui []                             = Nothing
lookupBr ui ((Branch uj e):bs) | ui == uj  = Just e
                               | otherwise = lookupBr ui bs  


prune :: Exp -> Exp
prune (Apply e' es)    = Apply (prune e') (map prune es)  
prune (Lambda is e')   = Lambda is (prune e')
prune (Case e' bs)     = Case (prune e') (map (\(Branch ui exp) -> (Branch ui (prune exp))) bs) 
prune (Rec j e')       = Rec j (prune e')
prune (Var j)          = Var j         
prune (Const ui)       = Const ui
prune _                = error "Invalid constructor"

-- In the following functions I use Apply (Const (UIdent _)) to represent
-- Natural numbers in X. I dont really know why they should be expressed with 
-- Apply. It should be enough with Const (UIdent _). However in the lecture notes
-- it is expressed with Apply, and thats why I implemented it like that here also.

codenat :: Int -> Exp
codenat n | n == 0    = Apply (Const (UIdent "zero")) []
          | otherwise = Apply (Const (UIdent "succ ")) ((codenat (check(n)-1)):[]) 

casenat :: Exp -> a -> (Exp -> a) -> a
-- casenat a b c = b   , if a is Z
-- casenat a b c = c n , if a is (S n)
casenat e a f | e == (Apply (Const (UIdent "zero")) []) = a
              | otherwise                               = f e 

decodenat :: Exp -> Int
decodenat (Apply (Const (UIdent _)) [])      = 0
decodenat (Apply (Const (UIdent _)) (x:xs))  = 1 + decodenat x
decodenat _                                  = error "invalid"

-- Helpfunction to check if an input integer is negative.
check x = if x<0 
          then error("Input number is negative")
          else x   

add = parse "rec add = \\x y. case x of { Z -> y; S -> \\n. (S (add n y))}"
-------------------------------------

printExp e = putStr ((printTree e)++"\n")

parse :: String -> Exp
parse s = case (pExp (myLexer s)) of
           Bad s2   -> error ("\nParse Failed...\n   " ++ s2)
           Ok  tree -> tree 
parsef file = do
        pString <- (readFile file) 
        printExp (parse pString)
