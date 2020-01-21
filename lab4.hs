--ex1--
polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord''(a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian''(MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r*cos phi, r*sin phi)

--ex2--
-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

xCoord'' :: Cart2DVec'' a -> a
xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

yCoord'' :: Cart2DVec'' a -> a
yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y


-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

-- enum type example (special case of sum type)
data ThreeColors = Blue |
                   White |
                   Red

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red   = "Irene Jacob"

{-
uwaga: ta sama nazwa* dla:
 - konstruktora typu (po lewej)
 - konstruktora danych/wartości (po prawej)

 * druga (obok omówionej poprzednio -- z prefiksem 'Mk') powszechna konwencja w Haskellu!
-}
data Cart3DVec a = Cart3DVec a a a

xCoord :: Cart3DVec -> Int
xCoord (Cart3DVec x _ _) = x

xCoord3D :: Cart3DVec a -> a
xCoord3D (MkCart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (MkCart3DVec _ y _) = y

zCoord3D :: Cart3DVec a -> a
zCoord3D (MkCart3DVec _ _ z) = z

--ex3--
data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt


data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"






--ex5--
data MyInt = MkMyInt Int
instance Eq MyInt where
    (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2
instance Ord MyInt where
    (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
    (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
    (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
    (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
    negate (MkMyInt i)            = MkMyInt (negate i)
    abs (MkMyInt i)               = MkMyInt (abs i)
    signum (MkMyInt i)            = MkMyInt (signum i)
    fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
    show (MkMyInt i) = "MkMyInt " ++ show i




    --ex7--
    module Stack (Stack(MkStack), empty, isEmpty, push, top, pop) where

    empty :: Stack a
    isEmpty :: Stack a -> Bool
    push :: a -> Stack a -> Stack a
    top :: Stack a -> a
    pop :: Stack a -> (a,Stack a)
    
    newtype Stack a = MkStack [a] deriving Show
    
    empty = MkStack []
    isEmpty (MkStack s) = null s
    push x (MkStack s) = MkStack (x:s)
    top (MkStack s) = head s
    pop (MkStack (s:ss)) = (s,MkStack ss)

    ex8--
    module Stack
    ( Stack
    , empty   -- :: Stack a
    , isEmpty -- :: Stack a -> Bool
    , push    -- :: a -> Stack a -> Stack a
    , top     -- :: Stack a -> a
    , pop     -- :: Stack a -> (a,Stack a)
    ) where
  
  -- interface (signature, contract)
  empty :: Stack a
  isEmpty :: Stack a -> Bool
  push :: a -> Stack a -> Stack a
  top :: Stack a -> a
  pop :: Stack a -> (a,Stack a)
  
  -- implementation
  newtype Stack a = MkStack [a] deriving Show -- hidden constructor (see the module export list)
  
  empty = MkStack []
  isEmpty (MkStack s) = null s
  push x (MkStack s) = MkStack (x:s)
  top (MkStack s) = head s
  pop (MkStack (s:ss)) = (s,MkStack ss)
