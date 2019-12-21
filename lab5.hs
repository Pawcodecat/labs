{-# LANGUAGE DeriveFunctor #-}

--ex2--
actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >> putChar '\n'

doActSeq = do
    putChar 'A'
    putChar 'G'
    putChar 'H'
    putChar '\n'

echol = getLine >>= putStrLn

doEchol = do
    line <- getLine
    putStrLn line


echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
    line <- getLine
    putStrLn $ line ++ "!"


echo3 :: IO ()
echo3 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO()
dialog = putStr "What is your happy number? "
        >>getLine 
        >>= \n -> let num = read n :: Int in
                  if num == 7
                  then putStrLn "Ah, lucky 7!"
                  else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

doEcho3::IO()
doEcho3 = do
    line1 <- getLine
    line2 <- getLine
    putStrLn $ line1 ++ line2

doDialog::IO()
doDialog = do 
        putStr "what is your happy number?"
        n <- getLine
        let num = read n :: Int in 
            if num == 7
            then putStrLn "Ah,Lucky 7!"
            else if odd num 
                 then putStrLn "Odd number"
                 else putStrLn "Unusual"


doTwoQuestions :: IO ()
doTwoQuestions = do 
    putStr "What is your name? "
    name <- getLine
    putStr "How old are you? "
    age <- getLine
    print (name,age)

twoQuestions::IO()
twoQuestions = putStr "What is your name?" >> getLine>>= \name -> putStr"How old are you" >> getLine >>= \age -> print(name,age)


--ex6--


newtype Box a = MkBox a deriving (Show, Functor)

-- instance Functor Box where 
--     fmap f (MkBox x) = MkBox (f x)

data MyList a = EmptyList 
                | Cons a (MyList a) deriving (Show, Functor)

-- instance Functor MyList where 
--     fmap _ EmptyList    = EmptyList
--     fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

data BinTree a = EmptyBT 
                | NodeBT a (BinTree a) (BinTree a)
                deriving (Show,Functor)

-- instance Functor BinTree where
--     fmap g EmptyBT          = EmptyBT
--     fmap g (NodeBT x lt rt) = NodeBT (g x)(fmap g lt)(fmap g rt)

--ex8--
newtype Box2 a = MkBox2 a deriving Show

instance Applicative Box2 where
    pure = MkBox2
    (MkBox2 f)<*> w = fmap f w

instance Functor Box2 where
    fmap f (MkBox2 x) = MkBox2 (f x)