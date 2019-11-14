--ex1--
f1  :: Double -> Double
f1  = \x -> x-2

f2 :: Double -> Double -> Double
f2 = \x y -> sqrt(x^2 +y^2)

f3 :: Double -> Double -> Double -> Double
f3 = \x y z -> sqrt(x^2 + y^2 + z^2)

f4 :: Double -> Double 
f4 = \ x -> 2*x

f5 :: Double -> Double 
f5 = \ x -> x*2

--f6 :: Double -> Double 
--f6 = \ x -> 2 ^ x 

f7 = \x -> if x `mod` 2 == 0 
    then True 
    else False

f8  = \x -> let y = sqrt x 
         in 2 * y^3 * (y + 1)

--ex2--
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumsqr' :: Num a => [a] -> a
sumsqr' []     = 0
sumsqr' (x:xs) = x*x + sum' xs
