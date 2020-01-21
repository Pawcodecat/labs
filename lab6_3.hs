import Control.Monad
import Control.Monad.Trans.Writer.Lazy
--ex3--
safeTail :: [a] -> Either String [a]
safeTail []      = Left "Empty list!"
safeTail (x:xs)  = Right xs


doSafeTail3x :: [a] -> Either String [a]
doSafeTail3x xs = do 
    t1 <- safeTail xs
    t2 <- safeTail t1
    t3 <- safeTail t2
    return t3


safeDiv :: Int -> Int -> Either String Int
safeDiv x y | y /= 0     = Right $ x `div` y
            | otherwise  = Left "Cannot div by zero!"

safeF5 :: Int -> Int -> Int -> Either String Int
safeF5 x y z =
    case (safeDiv 1000 x) of
        Left e -> Left e 
        Right (iOverX) ->
            case (safeDiv 100 y) of
                Left e -> Left e 
                Right (iOverY) ->
                    case(safeDiv 10 z) of
                        Left e -> Left e 
                        Right (iOverZ) -> Right $ iOverX + iOverY + iOverZ

safeF5' :: Int -> Int -> Int -> Either String Int
safeF5' x y z = do
    iOverX <- safeDiv 1000 x
    iOverY <- safeDiv 100 y 
    iOverZ <- safeDiv 10 z
    return $ iOverX + iOverY + iOverZ 

join :: Either b (Either b a) -> (Either b a)
join (Left x) = Left x
join (Right x) = x


--ex4--
xs1 :: [(Int,Int,Int)]
xs1 = [ (x,y,z) | let xs = [1,2], x<- xs, y<-xs, z<-xs]

doXs1 :: [(Int,Int,Int)]
doXs1 = do
    let xs = [1,2]
    x <- xs
    y <- xs
    z <- xs
    return (x,y,z)

xs2 :: [(Int,Int,Int)]
xs2 = [ (x,y,z) | let xs = [1..3], x <-xs, y <- xs, z <-xs, x>y && y > z]

doXs2 :: [(Int,Int,Int)]
doXs2 = do
  let xs = [1..3]
  x <- xs
  y <- xs
  z <- xs
  guard $ x > y && y > z
  return (x,y,z)

doXs2' :: [(Int,Int,Int)]
doXs2' = do
    let xs = [1..3]
    x <- xs
    y <- xs
    z <- xs
    if x>y && y>z 
        then return (x,y,z)
        else []

--ex5--


gcdWithLog :: Int -> Int -> Writer [String] Int
gcdWithLog a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcdWithLog b (a `mod` b)

mapWithLog :: Show a => (a-> b) -> [a] -> Writer [String] [b]
mapWithLog _ [] = do
    tell ["map []"]
    return []
mapWithLog f (x:xs) = do
    tell ["map " ++ show x]
    mapXs <- mapWithLog f xs
    return $ f x : mapXs