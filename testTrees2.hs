
import ShareVis

data Btree a = Node a (Btree a) (Btree a) | Leaf [a]
    deriving Foldable


-- nope, not gonna work
instance (Show a) => MemMappable (Btree a) where
    makeNode (Leaf (x:xs)) = (fst $ makeNode x, [(Leaf snd)])
    makeNode (Node x y z) = ((show x), [y, z])

t0 = Node "woo" (Node "bzz" (Leaf ["yaa", "nah"]) (Leaf ["boi"])) (Leaf ["yum"])

main = do showGraph [("t0", t0)]