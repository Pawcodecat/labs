--not' :: Bool->Bool
--not' b=case b of
--        True -> False
--       False -> True 
absInt :: Int->Int 
absInt n=
   case (n>= 0) of
      True -> n
      False-> -n 
isItTheAnswer :: String -> Bool
isItTheAnswer n=
                case(n=="Love") of
                  True -> True
                  False -> False
not' :: Bool -> Bool
not' n=
       case(n==True) of
         True -> False
         False -> True
or' :: (Bool, Bool) -> Bool
or' (m,n)=
     case(m==False && n==False) of
        True->False
        False->True
and' :: (Bool,Bool) -> Bool
and' (m,n)=
         case(m==True && n==True)of
         True->True
         False->False
nand' :: (Bool,Bool) ->Bool
nand' (m,n)=
         case(m==True && n==True)of
          True ->False
          False ->True
xor' :: (Bool,Bool) -> Bool
xor' (m,n)=
        case(m/=n)of
         True->True
         False->False