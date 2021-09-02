module Employee where

import Data.Semigroup
import Data.Tree

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun = Integer

-- An Employee consists of a name and a fun score.
data Employee =
  Emp
    { empName :: Name
    , empFun :: Fun
    }
  deriving (Show, Read, Eq)

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany =
  Node
    (Emp "Stan" 9)
    [ Node
        (Emp "Bob" 2)
        [ Node (Emp "Joe" 5) [Node (Emp "John" 1) [], Node (Emp "Sue" 5) []]
        , Node (Emp "Fred" 3) []
        ]
    , Node (Emp "Sarah" 17) [Node (Emp "Sam" 4) []]
    ]

testCompany2 :: Tree Employee
testCompany2 =
  Node
    (Emp "Stan" 9)
    [ Node
        (Emp "Bob" 3) -- (8, 8)
        [ Node
            (Emp "Joe" 5) -- (5, 6)
            [ Node (Emp "John" 1) [] -- (1, 0)
            , Node (Emp "Sue" 5) [] -- (5, 0)
            ]
        , Node (Emp "Fred" 3) [] -- (3, 0)
        ]
    , Node
        (Emp "Sarah" 17) -- (17, 4)
        [ Node (Emp "Sam" 4) [] -- (4, 0)
        ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList =
  GL [Employee] Fun
  deriving (Show, Eq)

employees :: GuestList -> [Employee]
employees (GL l _) = l

fun :: GuestList -> Fun
fun (GL _ f) = f

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ eFun) (GL gl fun) = GL (gl ++ [e]) (fun + eFun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL gl1 fun1) (GL gl2 fun2) = GL (gl1 ++ gl2) (fun1 + fun2)

instance Semigroup GuestList where
  (<>) = mappend

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 > f2 = gl1
  | otherwise = gl2

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f acc (Node val []) = f val acc
treeFold f acc (Node val children) = f val (treeFoldChildren f acc children)

treeFoldChildren :: (a -> b -> b) -> b -> [Tree a] -> b
treeFoldChildren f acc [] = acc
treeFoldChildren f acc ((Node val children):xs) =
  f val (treeFoldChildren f acc (children ++ xs))

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = (GL [e] (empFun e), GL [] 0)
nextLevel boss lowerList =
  ( GL
      (boss : employees bestLowerWithGrandBoss)
      (empFun boss + fun bestLowerWithGrandBoss)
  , bestLowerWithoutGrandBoss)
  where
    bestLowerWithGrandBoss =
      foldr1
        (<>)
        (map (\(gl1, gl2) -> moreFun (subtractBossFun gl1) gl2) lowerList)
    bestLowerWithoutGrandBoss = foldr1 (<>) (map (uncurry moreFun) lowerList)

subtractBossFun :: GuestList -> GuestList
subtractBossFun (GL [] _) = GL [] 0
subtractBossFun (GL all@(boss:employees) fun) = GL all (fun - empFun boss)

testCompany3 :: Tree Employee
testCompany3 =
  Node (Emp "Stan" 1) [Node (Emp "Bob" 2) [], Node (Emp "Sarah" 1) []]

testLowerList =
  [ (GL [Emp {empName = "Bob", empFun = 2}] 2, GL [] 0)
  , (GL [Emp {empName = "Sarah", empFun = 1}] 1, GL [] 0)
  ]

testBoss = Emp {empName = "A", empFun = 1}

--maxFun :: Tree Employee -> GuestList
maxFun company = treeFold (\e acc -> (nextLevel e acc) : acc) [] company
