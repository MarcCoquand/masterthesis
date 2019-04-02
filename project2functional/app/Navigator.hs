 module Navigator where


-- Zipper containing at least one element.
-- Zipper Left cursor Right
data Navigator a = Nav [a] a [a]


above :: Navigator a -> [a]
above (Nav l _ _) =
    reverse l


below :: Navigator a -> [a]
below (Nav _ _ r) =
    r


get :: Navigator a -> a
get (Nav _ element _) =
    element


up :: Navigator a -> Navigator a
up unchanged@(Nav [] _ _) =
    unchanged
up (Nav (l:ls) m r) =
    Nav ls l (m:r)


down :: Navigator a -> Navigator a
down unchanged@(Nav _ _ []) =
    unchanged
down (Nav l m (r:rs)) =
    Nav (m:l) r rs


delete :: Navigator a -> Maybe (Navigator a)
delete (Nav l _ (r:rs)) =
    Just (Nav l r rs)
delete (Nav (l:ls) _ []) =
    Just (Nav ls l [])
delete (Nav [] _ []) =
    Nothing


add :: Navigator a -> a -> Navigator a
add (Nav l m r) new =
    Nav (m:l) new r


update :: Navigator a -> a -> Navigator a
update (Nav l _ r) new =
    Nav l new r


init :: a -> Navigator a
init a =
    Nav [] a []


fromList :: [a] -> Maybe (Navigator a)
fromList [] =
    Nothing
fromList (a:as) =
    Just (Nav [] a as)

