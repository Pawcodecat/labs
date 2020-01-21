import Data.List

foldl3 :: (a -> b -> b) -> b -> [a] -> b
foldl3 f z []     = z
foldl3 f z (x:xs) = foldl3 f (z x) xs

sumWith'' g = foldl3 (\x acc -> g x + acc) 0 

prodWith'' g = foldl3 (\x acc -> g x * acc) 1