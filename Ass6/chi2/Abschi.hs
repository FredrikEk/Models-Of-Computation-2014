module Abschi where

-- Haskell module generated by the BNF converter


newtype UIdent = UIdent String deriving (Eq,Ord,Show)
newtype LIdent = LIdent String deriving (Eq,Ord,Show)
data Exp =
   Lambda [LIdent] Exp
 | Apply Exp [Exp]
 | Case Exp [Branch]
 | Rec LIdent Exp
 | Let [Def] Exp
 | Var LIdent
 | Const UIdent
  deriving (Eq,Ord,Show)

data Branch =
   Branch UIdent Exp
  deriving (Eq,Ord,Show)

data Def =
   Def LIdent Exp
  deriving (Eq,Ord,Show)

