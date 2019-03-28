

module Main where



{-# RULES
"fx"  forall x. f x = "a"
"lambda"  forall y a. h (\x -> a) y = a

#-}

{-# NOINLINE f #-}
{-# NOINLINE h #-}

h :: (Int -> Int) -> Int -> Int
h a b = 666

f :: [Char] -> [Char]
f x = x

g = f "b"

e = h (\x -> x) 2 -- 666
q = h (\x -> ((+) 4 x)) 2 -- 666
p = h (\x -> 5) 2 -- 5

main = do print $ e