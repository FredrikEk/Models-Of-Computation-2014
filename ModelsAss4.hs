--Fredrik Ek, 901109-0959

-- Ex 1
type PRFs = [Int] -> Int


zero :: PRFs 
zero [] = 0 
zero _  = error("Needs an empty list")

suc :: PRFs 
suc []    = error("List is empty")
suc (x:_) = (check x) + 1

proj :: Int -> PRFs 
proj i xs = findElement i xs  

compose :: PRFs -> [PRFs] -> PRFs 
compose f es xs = f (map (\e -> e xs) es)

rec' :: PRFs -> PRFs -> PRFs 
rec' e d (0:xs) = e xs
rec' e d (x:xs) = d ((x-1) : rec' e d ((x-1):xs) : xs)
 
add = rec' (proj 0)(compose suc [proj 1])
 
pre = rec' (zero) (proj 0) 

mul = rec' (compose zero []) (compose add [proj 1, proj 2]) 

fact = rec' (compose suc [zero]) 
               (compose mul [compose suc [proj 0], 
                            proj 1]) 

-- Ex 2

data PRF = Zero2
         | Succ2
         | Proj2 Int
         | Compose2 PRF [PRF]
         | Rec2 PRF PRF
         deriving Show
        
eval :: PRF -> PRFs
eval Zero2 _ = 0
eval Succ2 [x] = x+1
eval (Proj2 n) xs = findElement n xs
eval (Compose2 f es) xs = eval f (map (\e -> eval e xs) es)
eval (Rec2 e d) (0:xs) = eval e xs
eval (Rec2 e d) (x:xs) = eval d ((x-1) : eval (Rec2 e d) ((x-1):xs) : xs)

-- Helpfunctions --

-- Helpfunction to find an element in a list on a given index
findElement _ []    = error "List is empty, cant find element"                     
findElement 0 (x:_) = x
findElement (n) (_:xs) = findElement (n-1) xs

-- Helpfunction to check if an input integer is negative
check x = if x<0 
          then error("Input number is negative")
          else x         