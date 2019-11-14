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