--Fredrik Ek, 901109-0959

-- This program can be run and tested in the ghci using >:l ModelsAss2.hs and 
-- then proceed accordingly

-- Ex1
-- A Natural number can be either Zero or one (Succ Zero), two (Succ Succ Zero), ...
-- n (n*Succ Zero)
data Nat
  = Zero
  | Succ Nat

-- This tells what to do when trying to show our implemented datatype.
-- Can be tested by typing Succ Zero, Succ (Succ Zero), Succ (Succ (Succ Zero)) etc
instance Show Nat where
  show Zero     = "Zero"
  show (Succ n) = "Succ " ++ show n

-- Ex2
-- The higher-order function rec from the lecture notes
rec' :: Nat -> Nat -> (Nat -> Nat -> Nat) -> Nat
rec' Zero d e     = d
rec' (Succ a) d e = e (Succ a) (rec' a d e)

-- An add function for our implemented datatype. Can be tested typing for instance:
-- AddN (Succ Zero) (Succ Zero) which expresses 1+1.
addN :: Nat -> Nat -> Nat
addN a b = rec' a b (\x y -> Succ y)

-- Ex3

-- An implementation of a datatype for the set Z numbers. I tried to construct it using
-- our implemenation of Nat looking something like:
-- data Z
--   = Nat
--   | Neg Nat
-- It seemed to work at first, and it feels more intuitively correct, but I ran
-- into problems when trying to implement zrec where the program terminated everytime
-- I tried to use the rec function, for the case when the input Z wasn't negative.
-- It gave the error that it couldnt match expected type Nat->Nat with actual type Z->Z.
-- If I recall correct there should be a way of forcing the constructor to choose a
-- specific type, but I cant figure it out. It is however how I would have done it, 
-- if I knew how.

data Z
  = Zeroz
  | Succz Z
  | Neg Z
  
-- Yet again expressing how our implemented datatype should be showed on the 
-- screen when calling its constructor
-- Z-numbers can be expressed by typing for instance:
-- >Neg (Succz Zeroz) which represents -1,
-- >Succz Zeroz which represents 1, 
-- >Zeroz which represents 0 or
-- >Succz (Succz (Succz Zeroz)) all being in the set Z-numbers.
-- It should be trivial to realise that addition, subtraction and multiplication
-- can be implemented easily.
instance Show Z where
  show Zeroz     = "Zero"
  show (Succz n) = "Succ " ++ show n
  show (Neg n)   = "Neg (" ++ show n ++ ")"
  
-- Ex4
-- A modified version of the rec function for the set Z-numbers being analogous to
-- rec implemented above. 
zrec' :: Z -> Z -> (Z -> Z -> Z) -> Z
zrec' Zeroz d e     = d
zrec' a (Neg b) e   = e (Neg b) (zrec' a b e)
zrec' (Neg a) d e   = e (Neg a) (zrec' a d e)
zrec' (Succz a) b e = e (Succz a) (zrec' a b e)
  
  
-- Ex5
-- So my initial thought was to do something like 
-- If n is odd then (n+1)/2 else -n/2‏. 
-- This would give: 0 1 -1 2 -2 3 -3 which is in Z,
-- for the input in N; 0 1 2 3 4 5 6 etc. 
-- So it is obviously surjective because, in f:N->Z, for all Z there exists
-- an N such that f(x) = y
-- Implementing this would however require that I implement the arithmetic expressions
-- for my implemented data types. So I will make the function from Int->Int just to show
-- that it works. It is fine just to simulate the behaviour of a function like the one
-- described above. So lets assume the function ntoz only takes 
-- input arguments from N and outputs in Z.

ntoz :: Int -> Int
ntoz n = if odd n then div (n+1) 2 else div (-n) 2 

-- This function works as intended and its trivial to realise that if I were to 
-- implement odd, div and (+) for my data types, this would work for them also.
