import SimpleBinaryTrees


--Aufgabe 1
isFull :: SimpleBT -> Bool
isFull (N L L) = True
isFull (N L rt) = False
isFull (N lt L) = False
isFull (N lt rt) = (isFull lt) && (isFull rt) && height lt == height rt


numberLeaves :: SimpleBT->Integer
numberLeaves L = 1
numberLeaves (N lt rt) = (numberLeaves lt)+(numberLeaves rt)

insertLeaves :: Integer -> SimpleBT -> SimpleBT
insertLeaves 0 a = a
insertLeaves 1 L = N L L 
insertLeaves n L = N (insertLeaves (div n 2) L) (insertLeaves ((n-1)-(div n 2)) L)
insertLeaves n (N lt rt) | ((mod n 2)==0) = N (insertLeaves (div n 2) lt) (insertLeaves (div n 2) rt)
                         | (numberLeaves lt)>(numberLeaves rt) = N (insertLeaves ((div n 2)+1) lt) (insertLeaves (div n 2) rt)
                         | otherwise = N (insertLeaves (div n 2) lt) (insertLeaves ((div n 2)+1) rt)


removeLeaves :: Integer -> SimpleBT -> SimpleBT
removeLeaves 0 a = a
removeLeaves n (N L L) = L
removeLeaves n (N L rt) = N L (removeLeaves n rt)
removeLeaves n (N lt L) = N (removeLeaves n lt) L
removeLeaves n (N lt rt) | n>=(numberLeaves (N lt rt)) = error "The number is bigger(or equal) than numbers of leaves"
                         | (numberLeaves (N lt rt))-n==1 = L
                         | ((mod n 2)==0) && ((numberLeaves lt)>n) && ((numberLeaves rt)>n) = N (removeLeaves (div n 2) lt) (removeLeaves (div n 2) rt)
                         | ((mod n 2)/=0) && ((numberLeaves lt)>n) && ((numberLeaves rt)>n) = N (removeLeaves (div n 2) lt) (removeLeaves ((div n 2)+1) rt)
                         | ((numberLeaves lt)>n) = N (removeLeaves n lt) rt
                         | ((numberLeaves rt)>n) = N lt (removeLeaves n rt)
                         | otherwise = (removeLeaves (n-(numberLeaves lt)) rt)


tree4 = genSimpleBT 3
tree5 = insertLeaves 7 tree4

paint_t4 = printSimpleBT tree4
paint_t5 = printSimpleBT tree5

paint_t6 = printSimpleBT (removeLeaves 7 tree5)


--Aufgabe 2
data BSearchTree a = Nil | Node a (BSearchTree a) (BSearchTree a)
                                        deriving ( Show, Eq )

postOrder :: (Ord a) => BSearchTree a -> [a]
postOrder Nil = []
postOrder (Node x ltree rtree) = postOrder ltree++postOrder rtree++[x]

oneChild :: (Ord a) => BSearchTree a -> Bool
oneChild (Node x Nil Nil) = False
oneChild (Node x Nil rtree) = True 
oneChild (Node x ltree Nil) = True
oneChild (Node x ltree rtree) = (oneChild ltree) || (oneChild rtree)


height' :: BSearchTree a -> Integer
height' Nil = 0
height' (Node x lt rt) = (max (height' lt) (height' rt)) + 1

isFull' :: (Ord a) => BSearchTree a -> Bool
isFull' (Node x Nil Nil) = True
isFull' (Node x Nil rt) = False
isFull' (Node x lt Nil) = False
isFull' (Node x lt rt) = (isFull' lt) && (isFull' rt) && height' lt == height' rt

inOrder :: (Ord a) => BSearchTree a -> [a]
inOrder Nil = []
inOrder (Node x ltree rtree) = inOrder ltree ++ x : inOrder rtree

successor :: (Ord a) => a -> BSearchTree a -> Maybe a
successor  y (Node x lt rt) | length (dropWhile (<=y) (inOrder (Node x lt rt)))==0 = Nothing
                            | otherwise = Just (head (dropWhile (<=y) (inOrder (Node x lt rt))))
                                

mapTree :: (a -> b) -> BSearchTree a -> BSearchTree b
mapTree f Nil = Nil
mapTree f (Node x lt rt) = Node (f x) (mapTree f lt) (mapTree f rt)

test_mapTree = mapTree (^2) (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil)))

foldTree :: b -> (a -> b -> b -> b) -> BSearchTree a -> b
foldTree z f Nil = z
foldTree z f (Node x lt rt) = f x (foldTree z f lt) (foldTree z f rt)

addMult a b c = a + b*c
test_foldT = foldTree 0 addMult (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil)))


--Aufgabe 3
data Queue a = Queue [a] [a] 

enqueue :: a -> Queue a -> Queue a
enqueue b (Queue bs1 bs2) = Queue bs1 ([b]++bs2) 

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [] []) = error "The list is empty"
dequeue (Queue [] bs2) = dequeue (Queue (reverse bs2) [])
dequeue (Queue bs1 bs2) = (head bs1, Queue (tail bs1) bs2)

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _ = False

makeQueue :: Queue a 
makeQueue = Queue [] []

instance (Show a)=> Show (Queue a) where
    show (Queue bs1 bs2) = showQueue (Queue bs1 bs2)
                            where
                                showQueue (Queue bs1 bs2)= show (bs1++(reverse bs2))

instance (Eq a)=> Eq (Queue a) where
    (==) (Queue as1 as2) (Queue bs1 bs2) = (as1==bs1) && (as2==bs2)
    (/=) a b = not (a==b)

instance (Ord a)=> Ord (Queue a) where
    (<) (Queue as1 as2) (Queue bs1 bs2) = (<) (as1++(reverse as2)) (bs1++(reverse bs2))
    (>) (Queue as1 as2) (Queue bs1 bs2) = (>) (as1++(reverse as2)) (bs1++(reverse bs2))
    (<=) (Queue as1 as2) (Queue bs1 bs2) = (<=) (as1++(reverse as2)) (bs1++(reverse bs2))
    (>=)  (Queue as1 as2) (Queue bs1 bs2) = (>=) (as1++(reverse as2)) (bs1++(reverse bs2))
    

test_enqueue = enqueue 5 (Queue [1,2,3,4] [6,7,8,9])
test_dequeue = dequeue (Queue [1,2,3,4] [6,7,8,9])
test_makeQueue = makeQueue
test_isEmpty = isEmpty test_makeQueue
test_showQ = show (Queue [1,2,3,4] [6,7,8,9])
test_eq1Q = (Queue [1,2,3,4] [6,7,8,9]) == (Queue [1,2,3,4] [6,7,8,9])
test_eq2Q = (Queue [1,2,3,4] [6,7,8,9]) /= (Queue [1,2,3,4] [6,7,8,9])
test_ord1Q = (Queue [1,2,3,4] [6,7,8,9]) > (Queue [1,2,3,4] [6,7,8,9])
test_ord2Q = (Queue [1,2,3,4] [6,7,8,9]) < (Queue [1,2,3,4] [6,7,8,9])
test_ord3Q = (Queue [1,2,3,4] [6,7,8,9]) <= (Queue [1,2,3,4] [6,7,8,9])
test_ord4Q = (Queue [1,2,3,4] [6,7,8,9]) >= (Queue [1,2,3,4] [6,7,8,9])


--Aufgabe 4
data ABaum a = LA | NA a [ABaum a]


nodesA :: ABaum a -> Integer
nodesA LA = 1
nodesA (NA a bs) = 1+(foldr (+) 0 (map nodesA bs))

heightA :: ABaum a -> Integer
heightA LA = 0
heightA (NA a bs) = 1+ (foldr max 0 (map heightA bs))

mapTreeA :: (a -> b) -> ABaum a -> ABaum b
mapTreeA f LA = LA 
mapTreeA f (NA a bs) = NA (f a) (map (mapTreeA f) bs)

instance (Show a)=> Show (ABaum a) where
    show LA = "LA"
    show (NA a bs) = "(NA "++(show a)++" "++(show bs)


test_full1 = isFull tree2
test_full2 = isFull tree1
test_insert1 = printSimpleBT (insertLeaves 16 tree2)
test_insert2 = printSimpleBT (insertLeaves 20 tree3)
test_remove1 = printSimpleBT (removeLeaves 6 tree2)
test_remove2 = printSimpleBT (removeLeaves 16 tree2)
test_remove3 = printSimpleBT (removeLeaves 20 tree3)

test_order = postOrder (Node 6 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 5 Nil Nil)) (Node 9 (Node 7 Nil (Node 8 Nil Nil)) (Node 10 Nil Nil)))
--[1,3,2,5,4,8,7,10,9,6] erwartet
test_child1 = oneChild (Node 6   (Node 4 (Node 2  Nil Nil)   (Node 5 Nil Nil))                   (Node 9 (Node 7 Nil Nil)       (Node 10 Nil Nil)))
test_child2 = oneChild (Node 6 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 5 Nil Nil)) (Node 9 (Node 7 Nil (Node 8 Nil Nil)) (Node 10 Nil Nil)))
test2_Full1 = isFull' (Node 6 (Node 4 (Node 2  Nil Nil) (Node 5 Nil Nil)) (Node 9 (Node 7 Nil Nil) (Node 10 Nil Nil)))
test2_Full2 = isFull' (Node 6 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 5 Nil Nil)) (Node 9 (Node 7 Nil (Node 8 Nil Nil)) (Node 10 Nil Nil)))
test_successor1 = successor 6 (Node 6 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 5 Nil Nil)) (Node 9 (Node 7 Nil (Node 8 Nil Nil)) (Node 10 Nil Nil)))
test_successor2 = successor 10 (Node 6 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 5 Nil Nil)) (Node 9 (Node 7 Nil (Node 8 Nil Nil)) (Node 10 Nil Nil)))
test_successor3 = successor 13 (Node 6 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 5 Nil Nil)) (Node 9 (Node 7 Nil (Node 8 Nil Nil)) (Node 10 Nil Nil)))
test_successor = successor 9 (Node 6 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 5 Nil Nil)) (Node 9 (Node 7 Nil (Node 8 Nil Nil)) (Node 10 Nil Nil)))
test_map1 = mapTree (^2) (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil)))
test_map2 = mapTree (+3) (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil)))
test_map3 = mapTree (show) (Node 6 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 5 Nil Nil)) (Node 9 (Node 7 Nil (Node 8 Nil Nil)) (Node 10 Nil Nil)))
test_fold1 = foldTree 0 addMult (Node 5 (Node 3 Nil Nil) (Node 8 Nil (Node 4 Nil Nil)))
test_fold2 = foldTree 0 addMult (Node 6 (Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 5 Nil Nil)) (Node 9 (Node 7 Nil (Node 8 Nil Nil)) (Node 10 Nil Nil)))

test_nodesA1 = nodesA (NA 2 [NA 3 [LA]])
test_nodesA2 = nodesA (NA 2 [(NA 3 [(NA 5 [LA]),(NA 6 [LA]) ]), (NA 4 [LA])])
test_heightA = heightA (NA 2 [(NA 3 [(NA 5 [LA]),(NA 6 [LA]) ]), (NA 4 [LA])])
test_mapA = mapTreeA (+2) (NA 2 [(NA 3 [(NA 5 [LA]),(NA 6 [LA]) ]), (NA 4 [LA])])