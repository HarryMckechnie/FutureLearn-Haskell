data Tree = Leaf | Node Int Tree Tree deriving Show 

-- an integer tree defined for function testing purposes
myTree :: Tree
myTree = Node 8 (Node 4 (Node 3 Leaf Leaf) Leaf) (Node 6 Leaf (Node 5 Leaf Leaf))

-- longest path from root to a leaf (already suppled)
-- eg treeDepth myTree
treeDepth :: Tree -> Int 
treeDepth Leaf = 0 
treeDepth (Node _ leftSubtree rightSubtree) =  
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree) 

-- treeSum gives the sum of all the nodes of a tree of integers
-- eg treeSum myTree
treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node x left right) = x + treeSum left + treeSum right

-- tree2List converts a tree of integers to a list of integers
-- eg tree2List myTree
tree2List :: Tree -> [Int]
tree2List Leaf = []
tree2List (Node x left right) = [x] ++ tree2List left ++ tree2List right

-- insertNum inserts an integer into a tree of integers at the shortest path node
-- it is not a destructive change to myTree
-- eg insertNum 9 myTree
insertNum :: Int -> Tree -> Tree
insertNum x Leaf = Node x Leaf Leaf
insertNum x (Node y left right)
  |treeDepth left == 0 = Node y (Node x Leaf Leaf) right
  |treeDepth right == 0 = Node y left (Node x Leaf Leaf)
  |treeDepth left <= treeDepth right = Node y (insertNum x left) right
  |treeDepth right < treeDepth left = Node y left (insertNum x right)

-- list2Tree converts  a list of integers to a tree of integers
-- eg list2Tree [1,2,3,4,5]
list2Tree :: [Int] -> Tree
list2Tree [] = Leaf
list2Tree lst = insertNum a (list2Tree b) where (a:b) = last lst: init lst