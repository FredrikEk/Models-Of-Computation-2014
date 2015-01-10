module Eval where
import Data.List
import Data.Maybe
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import Lexchi
import Parchi
import Skelchi
import Printchi
import Abschi
import ErrM

substBranch :: Branch -> LIdent -> Exp -> Branch
substBranch (Branch i e) v t = Branch i (subst e v t)

substDef :: Def -> LIdent -> Exp -> Def
substDef (Def i e) v t | i == v = Def i e
                       | otherwise = Def i (subst e v t)

substs :: Exp -> [LIdent] -> [Exp] -> Exp
substs e is es = foldl (\e (v, s) -> subst e v s) e (is `zip` es)

subst :: Exp -> LIdent -> Exp -> Exp
subst (Lambda is e) v t | v `elem` is = Lambda is e
                        | otherwise = Lambda is (subst e v t)
subst (Apply e es) v t = Apply (subst e v t) (map (\s -> subst s v t) es)
subst (Case e bs) v t = Case (subst e v t) (map (\b -> substBranch b v t) bs)
subst (Rec i e) v t | i == v = Rec i e
                    | otherwise = Rec i (subst e v t)
subst (Var i) v t | i == v = t
                  | otherwise = Var i
subst (Const ui) v t = Const ui

lookupBr :: UIdent -> [Branch] -> Maybe Exp
lookupBr ui []                             = Nothing
lookupBr ui ((Branch uj e):bs) | ui == uj  = Just e
                               | otherwise = lookupBr ui bs

isExpInBranch :: Maybe Exp -> Exp
isExpInBranch me = if isJust me then fromJust me else error "Contains nothing"

printExp e = putStr ((printTree e)++"\n")
-------------------------------------


-- 1a.
parse :: String -> Exp
parse s = case (pExp (myLexer s)) of
           Bad s2   -> error ("\nParse Failed...\n   " ++ s2)
           Ok  tree -> tree 

-- 1b.
pp :: Exp -> String 
pp e = ((printTree e)++"\n")

-- 1c.
evaltot :: Exp -> Exp
evaltot (Lambda is e)  = (Lambda is e)
evaltot (Apply e es) = (Apply e es)
evaltot e = evaltot (eval e)

eval :: Exp -> Exp
eval (Apply e es) = case eval e of  
						Lambda is g -> eval (substs g is es)
						Apply x ds  -> case x of 
										Const i -> Apply x (ds ++ es)
eval (Case e t) = case eval e of 
						Apply x es -> case x of 
								 		Const i -> eval (Apply (isExpInBranch (lookupBr i t)) es)
eval (Rec i e) = eval (subst e i (Rec i e))
eval (Lambda is e) = Lambda is e
eval (Const i) = Apply (Const i) []

-- 1d.
-- a can be tested with 'parse add', b with 'pp (parse add)' and c with 'eval (parse add)'  
-- or evaltot (eval (parse add)) etc, in the ghci environment upon loading the file Eval.hs; :l Eval.hs
add = "rec add = \\x y . case x of {" ++ 
							"Z -> y;" ++ 
							"S -> \\n . (S (add n y))}"

----------------------------------------
parsef file = do
        pString <- (readFile file) 
        printExp (parse pString)

---Ex 2

data PExp =
   PLambda [LIdent] Exp
 | PApply Exp [Exp]
 | PVar LIdent
 | PConst UIdent
  deriving (Eq,Ord,Show)

primrec :: PExp -> PExp -> (PExp->PExp->PExp) -> PExp
primrec e g f | e == (PConst (UIdent "Z")) = g
primrec e g f | otherwise = f (PConst (UIdent "Succ " a)) (primrec (PConst (UIdent "Succ a")) g f)

evalp :: PExp -> PExp 
evalp = error "Not implemented yet"

evaltotp :: PExp -> PExp
evaltotp = error "Not implemented yet"