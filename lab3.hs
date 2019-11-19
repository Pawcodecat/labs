import Data.List
import Data.Char

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

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f []  = 0
sumWith f (x:xs) = f x + sumWith f xs

sum1 = sumWith (\e -> e)
sumSqr = sumWith (\e -> sqrt(e))
sumCube = sumWith (\e -> e^3)
sumAbs = sumWith (\e -> abs e)

listLength = sumWith (\e -> 1) 

--ex3--
sqr x = x^2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^3)
    4 -> \x -> x^4
    5 -> intFunc
    _ -> const n
    where 
        intFunc x = x^5

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = case n of
    0 -> \x -> 1
    1 -> \x -> 1 + x
    2 -> \x -> 1 + x + (x^2)/2
    3 -> \x -> 1 + x + (x^2)/2 + (x^3)/6
    4 -> \x -> 1 + x + (x^2)/2 + (x^3)/6 + (x^4)/24
    5 -> \x -> 1 + x + (x^2)/2 + (x^3)/6 + (x^4)/24 + (x^5)/120

--ex4--
funcList :: [Double -> Double]
funcList = [\x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1)**x]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs 

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2*t,\t -> 3 * t^2)

--ex5--
sortDesc :: Ord a => [a] -> [a]
sortDesc xs = reverse (sort xs )

--ex7--
onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p (x:xs) | p x        = x : filter' p xs
                 | otherwise  = filter' p xs
filter' p []                     = []
    
onlyEven2 = filter' even
onlyOdd2 = filter' odd
onlyUpper2 = filter' isUpper       -- ? ????????? nie dziala

filter2 p xs = [ x | x <- xs, p x]

--ex8--
doubleElems []    = []
doubleElems (x:xs) = 2 *x : doubleElems xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map f xs 

doubleElems2 = map' (\x -> 2*x)
sqrElems2 = map' (\x -> x^2)
lowerCase2 = map' toLower 

doubleElems3 xs = [2*x | x <- xs]
sqrElems3 xs = [x^2 | x <- xs ]
lowerCase3 xs = [toLower x | x <- xs ]

--ex9--
sumWith2 g []     = 0
sumWith2 g (x:xs) = g x + sumWith2 g xs -- (+) (g x) (sumWith g xs)

prodWith2 g []     = 1
prodWith2 g (x:xs) = g x * prodWith2 g xs -- (*) (g x) (prodWith g xs)
 