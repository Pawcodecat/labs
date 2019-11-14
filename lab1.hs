--ex2--
printHello = putStrLn "Hello"

--ex4--
sqr :: Double -> Double
sqr x=x*x
vec2DLen :: (Double, Double) -> Double
vec2DLen (x,y)=sqrt(x^2+y^2)
vec3DLen :: (Double,Double,Double)->Double
vec3DLen (x,y,z)=sqrt(x^2 + y^2 + z^2)
f1 :: (Double,Double,Double)->(Double,Double)
f1 (x,y,z)=(x+y+z, x^2+y^2+z^2)
swap :: (Int,Char)->(Char,Int)
swap (x,y)=(y,x)
threeEqual :: (Int, Int, Int)->Bool
threeEqual (x,y,z)=(x==y)&&(y==z)

--ex5--
sgn :: Int->Int
sgn n = if n < 0
        then -1
        else if n == 0
                then 0
                else 1
min2Int :: (Int, Int) -> Int -- min (1,2) = 1, min (-1, -1) = -1
min2Int (x,y) = if x < y
                then x
                else y

--ex6--
absInt :: Int->Int
absInt n | n >= 0 = n 
         | otherwise = -n
sgn2 :: Int->Int
sgn2 x | x > 0 = 1
      | x < 0 = -1
      | x == 0 = 0
min3Int :: (Int, Int, Int)->Int
min3Int (x,y,z) | x>=z && y>=z =z
                | z>=x && y>=x =x
                | z>=y && x>=y =y

--ex7--
not' :: Bool -> Bool
not' True = False
not' False = True
isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer  _ = False
or' :: (Bool,Bool) -> Bool
or' (True,True) = True
or' (True,False) = True
or' (False,True) = True
or' (False,False) = False

and' :: (Bool,Bool) -> Bool
and' (True,True) = True
and' (True,False) = False
and' (False,True) = False
and' (False,False) = False

nand' :: (Bool,Bool) -> Bool
nand' (True,True) = False
nand' (True,False) = True
nand' (False,True) = True
nand' (False,False) = True

xor' :: (Bool,Bool) -> Bool
xor' (True,True) = False
xor' (True,False) = True
xor' (False,True) = True
xor' (False,False) = False

--ex8--
--not' :: Bool->Bool
--not' b=case b of
--        True -> False
--       False -> True 
absInt2 :: Int->Int 
absInt2 n=
   case (n>= 0) of
      True -> n
      False-> -n 
isItTheAnswer2 :: String -> Bool
isItTheAnswer2 n=
                case(n=="Love") of
                  True -> True
                  False -> False
not2' :: Bool -> Bool
not2' n=
       case(n==True) of
         True -> False
         False -> True
or2' :: (Bool, Bool) -> Bool
or2' (m,n)=
     case(m==False && n==False) of
        True->False
        False->True
and2' :: (Bool,Bool) -> Bool
and2' (m,n)=
         case(m==True && n==True)of
         True->True
         False->False
nand2' :: (Bool,Bool) ->Bool
nand2' (m,n)=
         case(m==True && n==True)of
          True ->False
          False ->True
xor2' :: (Bool,Bool) -> Bool
xor2' (m,n)=
        case(m/=n)of
         True->True
         False->False

--ex9--
roots :: (Double, Double, Double)->(Double,Double)
roots (a,b,c) = ((-b-d)/e,(-b+d)/e)
   where d = sqrt(b*b - 4*a*c) 
         e = 2*a 
unitVec2D :: (Double,Double)->(Double,Double)
unitVec2D (x,y)=(x/a,y/a) 
    where a=sqrt(x^2+y^2)

--ex10--
roots2 :: (Double,Double,Double)->(Double,Double)
roots2 (a,b,c)= 
         let d=sqrt(b*b-4*a*c)
             e=2*a
         in ((-b-d)/e,(-b+d)/e)
unitVec2D2 :: (Double,Double)->(Double,Double)
unitVec2D2 (a,b)=
             let l=sqrt(a^2+b^2)
             in (a/l,b/l)

--ex11--
roots3 :: (Double,Double,Double)->(Double,Double)
roots3 (a,b,c)=((-b-d)/e,(-b+d)/e) 
        where d=sqrt(b*b - 4*a*c)
              e=2*a