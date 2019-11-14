--ex1--

myFun x = 2 * x

subtract2C :: Num a => a -> (a -> a)   --spytać się na flipe2--
subtract2C x y = x - y

add2T :: Num a => (a,a) -> a
add2T (x,y) = x + y

add2C :: Num a => a -> (a -> a) 
add2C x y = x + y

add2C2 :: Num a => a -> a -> a 
add2C2 x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (x,y,z) = x + y + z

add3C :: Num a => a -> (a -> a -> a)
add3C  x y z = x + y + z

add3C2 :: Num a => a -> a -> a -> a
add3C2  x y z = x + y + z

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f x y = f(x, y)

curry2' :: ((a, b) -> c) -> (a -> b -> c)
curry2' f = \x y -> f (x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (x,y) = f x y

uncurry2' :: (a -> b -> c) -> (a, b) -> c
uncurry2' f = \(x,y) -> f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

curry3' :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3' f = \x y z -> f (x,y,z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

uncurry3' :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3' f = \(x,y,z) -> f x y z

--ex2--
fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)               -- fiveToPower_ 3 = 125

_ToPower5 :: Num a => a -> a
_ToPower5 = (^ 5)               -- _ToPower5 2 = 32

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 =(5 -)                -- subtrNFrom5 3 = 2

subtr5From_ :: Num a => a -> a
subtr5From_ = flip (-) 5             -- subtr5From_ 6 = 1

flip2 :: (a -> b -> c) -> b -> a -> c
flip2 f x y = f y x

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 f x y z= f z y x

--ex4--

isPalindrome :: [Char] -> Bool
isPalindrome s = if(s == reverse s)
                 then True
                 else False

--getElementAtIdx :: [a] -> b -> c             --spytać się?--
--getElementAtIdx xs x = head (drop x xs)

--upperFirst :: String -> String
--upperFirst (c1:c2:rest) =
--    if isSpace c1 && isLower c2
--        then c1 : toUpper c2 : upperFirst rest
--        else c1 : upperFirst (c2:rest)
--upperFirst s = s


--ex5--

isPrime :: Integral t => t -> Bool
isPrime n = if ((length [i | i <- [2..n^2], n `mod` i == 0]) > 1)
            then False
            else True

--ex6--
fib :: (Num a, Eq a)=> a -> a
fib n=
    if n==0 || n==1 
    then n 
    else fib(n-2) + fib(n-1)

sum' :: Num a => [a]->a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] =0
length' (x:xs) = 1 + length'(xs)

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' []=False
or' (x:xs) = x || or'(xs)

and' :: [Bool] -> Bool -- and' [True, False, True] = False
and' [] = True
and' (x:xs) = x && and'(xs)

elem' :: Eq a => a -> [a] -> Bool -- elem' 3 [1,2,3] = True
elem' e [] = False
elem' e (x:xs) 
        | (x == e) = True 
        | otherwise = elem' e xs

doubleAll :: Num t => [t] -> [t] -- doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll (x:xs) = [2*x] ++ (doubleAll(xs))

squareAll :: Num t => [t] -> [t] -- squareAll [2,3] = [4,9]
squareAll [] = []
squareAll (x:xs) = [x * x] ++ squareAll(xs)

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven (x:xs)
                | x `mod` 2 == 0 =[x] ++ selectEven(xs)
                | otherwise =[] ++ selectEven(xs)

--ex7--
sum'2 :: Num a => [a] -> a 
sum'2 xs = loop 0 xs 
        where loop acc []     = acc
              loop acc (x:xs) = loop (x + acc) xs

{-sum'3 :: Num a => [a] -> a
  sum'3 loop 0
        where loop acc []       = acc
              loop acc (x:xs)  = loop (x + acc) xs
 -}

prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
            where loop acc []     = acc
                  loop acc (x:xs) = loop(acc * x) xs

 --prod2.1 :: Num a => [a] -> a
 --prod2.1 a = loop 1 a
 --            where loop acc []     = acc
 --                  loop acc (x:xs) = loop(acc * x) xs



length'2 :: [a] -> Int
length'2 xs = loop 0 xs
                 where loop acc  []    = acc
                       loop acc (x:xs) =loop(acc + 1) xs  
 
--ex9--

qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where leftPart  xs = [ y | y <- xs, y <= x ]
       rightPart xs = [ y | y <- xs, y > x  ]
   
quicksort1 :: (Ord a) => [a] -> [a]
quicksort1 [] = []
quicksort1 (x:xs) =
    let smallerSorted = quicksort1 [a | a <- xs, a <= x]
        biggerSorted = quicksort1 [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted     
    
qSort2 :: Ord a => [a] -> [a]
qSort2 [] = []
qSort2 (x:xs) = qSort (leftPart) ++ [x] ++ qSort (rightPart)
 where leftPart  = filter (<x) xs
       rightPart  = filter (>=x) xs
      
 --ex10--      
--fst2Eq :: Eq a => [a] -> Bool
--fst2Eq (x : y : ) | x == y = True
--fst2Eq                    = False

--fst2Div :: Eq a => [a] -> Bool
--fst2Div (x : y : ) | (y `mod` x == 0) = True
--fst2Div                             = False