module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st var val = helper
  where
    helper key 
      | key == var = val 
      | otherwise = st key


empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st expr = 
  case expr of 
     Var s -> st s 
     Val i -> i
     Op e1 b e2 -> 
       case b of 
         Plus -> evalE st e1 + evalE st e2
         Minus -> evalE st e1 - evalE st e2
         Times -> evalE st e1 * evalE st e2
         Divide -> evalE st e1 `div` evalE st e2
         Gt -> if evalE st e1 > evalE st e2 then 1 else 0 
         Ge -> if evalE st e1 >= evalE st e2 then 1 else 0 
         Lt -> if evalE st e1 < evalE st e2 then 1 else 0 
         Le -> if evalE st e1 <= evalE st e2 then 1 else 0 
         Eql -> if evalE st e1 == evalE st e2 then 1 else 0 
-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar st = 
  case st of 
    Assign s e -> DAssign s e
    Incr s -> DAssign s (Op (Var s) Plus (Val 1))
    If e s1 s2 -> DIf e (desugar s1) (desugar s2)
    While e s -> DWhile e (desugar s)
    For s1 e s2 s3 -> desugar (Sequence s1 (While e (Sequence s2 s3)))
    Sequence s1 s2 -> DSequence (desugar s1) (desugar s2)
    _ -> DSkip


-- Exercise 4 -----------------------------------------
evalSimple :: State -> DietStatement -> State
evalSimple st ds = 
  case ds of 
    DAssign s e -> extend st s (evalE st e)
    DIf e ds1 ds2 -> 
      if evalE st e == 1 
        then evalSimple st ds1
        else evalSimple st ds2
    DWhile e while@(DWhile expr ds) -> 
      if evalE st e == 1 then 
        evalSimple st (DSequence ds while)
      else 
        st
    DSequence ds1 ds2 -> evalSimple (evalSimple st ds1) ds2 
    _ -> st

run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]

runFact :: State
runFact = run (extend empty "In" 4) factorial

runSqrt :: State
runSqrt = run (extend empty "In" 16) squareRoot

runFibo :: State
runFibo = run (extend empty "In" 4) fibonacci