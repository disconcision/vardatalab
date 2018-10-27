{-# LANGUAGE BangPatterns #-}

import ShareVis

data Btree a = Node a (Btree a) (Btree a) | Leaf a

instance (Show a) => MemMappable (Btree a) where
    makeNode (Leaf x) = ((show x), [])
    makeNode (Node x y z) = ((show x), [y, z])

t0 = Node "woo" (Node "bzz" (Leaf "yaa") (Leaf "boi")) (Leaf "yum")

main = do showGraph [("t0", t0)]