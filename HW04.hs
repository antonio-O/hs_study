{-# OPTIONS_GHC -Wall #-}
module HW04 where

newtype Poly a = P [a]
-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
		(==) (P x)(P y) = (xp==yp)
			where
				xp = x ++	replicate (max (length x) (length y) - length x) 0
				yp = y ++ replicate (max (length x) (length y) - length y) 0 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
	show x@(P xs)
		|(x == P[0]) = "0"
		|otherwise = concat' terms
		
			where 
			terms = filter (\x -> x /= "") monos
			
			concat' :: [String]->String
			concat' [x] = x
			concat' (x:xs) = concat'(xs) ++ " + " ++ x
			monos = map (\(x,y) -> mono x y) (zip xs [0..(length xs)-1])
				where

					mono :: (Num a, Eq a, Show a) => a -> Int ->String
					mono 0 d = ""
					mono c 0 = show(c)
					mono 1 1 = "x"
					mono (-1) 1 = "-x"
					mono c 1 = show(c)++"x"
					mono (-1) d = "-x^"++show(d)
					mono 1 d = "x^"++show(d) 
					mono c d = show(c)++"x^"++show(d) 		


-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a 
plus (P xs) (P ys) = P (map (\(x,y)->x+y) (zip xp yp))
	where
		xp = xs ++ replicate (max (length xs) (length ys) - length xs) 0
		yp = ys ++ replicate (max (length xs) (length ys) - length ys) 0 

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P as) (P bs) = foldr plus (P [0]) polys
	where
		mult :: Num a => a -> Int -> Poly a -> Poly a
		mult c d (P	xs) =	P (map (*c) ((replicate d 0) ++ xs))
		polys = map (\(x,y) -> mult x y (P bs)) (zip  as [0..(length as)-1])
	

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P xs) = P (map negate xs) 
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

