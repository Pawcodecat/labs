data Cart3DVec a = MkCart3DVec {x::a, y::a, z::a} deriving Show

data Cart3DVec a = Cart3DVec a a a

 xCoord :: Cart3DVec a -> a
 xCoord (Cart3DVec x _ _)  = x

data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = r * r * pi
area (Rectangle a b) = a * b

data Tree a = EmptyT | Node a (Tree a) (Tree a) deriving Show

rootValue :: Tree a -> a
rootValue (EmptyT) = error "Empty tree dumbass!!"
rootValue (Node root _ _) = root


data TrafficLights = Red | Orange | Green

actionFor :: TrafficLights -> String
actionFor Red = "Stop!"
actionFor Orange = "Slow down!"
actionFor Green = "Go for it!"


data BinIntTree = EmptyIntBt | IntNodeBt Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBt = 0
sumBinIntTree (IntNodeBt root left right) = 
    root + (sumBinIntTree left) + (sumBinIntTree right)


data Expr a = Lit a | Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2


show' :: Show a => Expr a -> String
show' (Lit n) =  show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"


data BinTree a = EmptyTree | TreeNode a (BinTree a) (BinTree a) 

depthOfBT :: BinTree a -> Int
depthOfBT EmptyTree = 0
depthOfBT (TreeNode _ left right) = 
    1 + max (depthOfBT left) (depthOfBT right)


elemOf :: Ord a => a -> BinTree a -> Bool
elemOf _ EmptyTree = False
elemOf e (TreeNode root left right) = 
    if e == root then
        True
    else
        if e < root then
            elemOf e left
        else
            elemOf e right

test = TreeNode 5 (EmptyTree) (TreeNode 8 EmptyTree EmptyTree)

reflect :: BinTree a -> BinTree a
reflect EmptyTree = EmptyTree
reflect (TreeNode root left right) = 
    TreeNode root (reflect right) (reflect left) 


inorderTraversal :: BinTree a -> [a]
inorderTraversal EmptyTree = []
inorderTraversal (TreeNode root left right) = 
    (inorderTraversal left) ++ [root] ++ (inorderTraversal right)


preorderTraversal :: BinTree a -> [a]
preorderTraversal EmptyTree = []
preorderTraversal (TreeNode root left right) = 
    root : (preorderTraversal left) ++ (preorderTraversal right)

postorderTraversal :: BinTree a -> [a]
postorderTraversal EmptyTree = []
postorderTraversal (TreeNode root left right) = 
    (postorderTraversal left) ++ (postorderTraversal right) ++ [root]

mapBt :: (a -> b) -> BinTree a -> BinTree b
mapBt _ EmptyTree = EmptyTree
mapBt mapper (TreeNode root left right) =
     TreeNode (mapper root) (mapBt mapper left) (mapBt mapper right)


insert :: Ord a => a -> BinTree a -> BinTree a 
insert e EmptyTree = (TreeNode e EmptyTree EmptyTree)
insert e (TreeNode root left right) = 
    if e < root then
        (TreeNode root (insert e left) right)
    else    
        (TreeNode root left (insert e right))


     
list2BST :: Ord a => [a] -> BinTree a 
list2BST [] = EmptyTree
list2BST xs = 
    let 
        middle (x:[]) = x
        middle xs = middle $ tail $ init xs
        len = length xs
        left = take (len `div` 2) xs
        right = drop (len `div` 2 + 1) xs
    in 
        TreeNode (middle xs) (list2BST leftSubTree) (list2BST rightSubTree)