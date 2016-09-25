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
extend st s num = nextst
	where
		nextst var
			| (var==s) = num
			| otherwise = (st var)
empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var s) = st s
evalE st (Val num) = num
evalE st (Op exp1 bop exp2) = evalBop bop (evalE st exp1) (evalE st exp2)
	where
		evalBop :: Bop -> Int -> Int -> Int
		evalBop Plus n1 n2 = n1 + n2	
		evalBop Minus n1 n2 = n1 - n2	
		evalBop Times n1 n2 = n1 * n2
		evalBop Divide n1 n2 = n1 `div` n2	
		evalBop Gt n1 n2 = if n1 > n2 then 1 else 0 
		evalBop Ge n1 n2 = if n1 >= n2 then 1 else 0
		evalBop Lt n1 n2 = if n1 < n2 then 1 else 0
		evalBop Le n1 n2 = if n1 <= n2 then 1 else 0
		evalBop Eql n1 n2 = if n1==n2 then 1 else 0
-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s exp) = DAssign s exp
desugar (Incr s) = DAssign s (Op (Var s) Plus (Val 1))
desugar (If exp st1 st2) = DIf exp (desugar st1) (desugar st2)
desugar (While exp st) = DWhile exp (desugar st)
desugar (For init exp update loop) = DSequence (desugar init) (DWhile exp (DSequence (desugar loop) (desugar update)))
desugar (Sequence st1 st2) = DSequence (desugar st1) (desugar st2)
desugar Skip = DSkip 

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign s exp) = extend state s (evalE state exp)
evalSimple state (DIf exp dst1 dst2)
	| val == 1 = evalSimple state dst1
	| otherwise = evalSimple state dst2
	where
		val = evalE state exp
evalSimple state while@(DWhile exp dst)
	| val == 1 = evalSimple (evalSimple state dst) while
	| otherwise = state
	where
		val = evalE state exp
evalSimple state (DSequence dst1 dst2) = evalSimple (evalSimple state dst1) dst2
evalSimple state DSkip = state

run :: State -> Statement -> State
run state st = evalSimple state (desugar st)

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
