
import ShareVis

lx = []
a = ["1", "2", "3", "4", "5"]
b = tail a
c = reverse a
d = "one" : b

main = do showGraph [("a", a), ("b", b), ("c", c), ("d", d)]