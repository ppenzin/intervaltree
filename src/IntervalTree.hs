module IntervalTree where

{- Primitives for boundaries and intervals -}
data Bound a = Incl a | Excl a
   deriving Show
data Interval a = Interval {low :: Bound a, high :: Bound a}
   deriving Show

{- Check if boundaries complement each other -}
complementary :: (Ord a) => Bound a -> Bound a -> Bool
complementary (Incl _) (Incl _) = False
complementary (Excl _) (Excl _) = False
complementary (Excl a) (Incl b) = (a == b)
complementary (Incl b) (Excl a) = (a == b)

{- Check is intervals are adjacent
 - arguments are given in order the intervals are ment to be
 -}
adjacent :: (Ord a) => Interval a -> Interval a -> Bool
adjacent (Interval _ a) (Interval b _) = (complementary a b)

{- Interval tree stores intervals in leafs and branches and allows adding data
 - of any type to each interval. Intervals have to constitute countinuous interval
 - that would span the whole tree
 - Interval tree is a balanced tree.
 -}
data IntervalTree a b = ILeaf {value :: b, interval :: Interval a}
   | IBranch { value :: b, interval :: Interval a, left :: IntervalTree a b, right:: IntervalTree a b}
   | INil
   deriving Show

{- Create a tree consisting of a single interval and a value -}
simpleTree :: (Ord a) => Interval a -> b -> IntervalTree a b
simpleTree int val = ILeaf {value = val, interval = int}

{- Check that a list of interval and data tuples contains adjacent 
 - intervals
 - Raises an error when two neighboring intervals are not adjacent.
 -}
checkList :: (Ord a) => [(Interval a, b)] -> [(Interval a, b)]
checkList = undefined -- XXX

{- Create an interval tree from list
 - TODO implement checking for adjacency
 -}
fromList :: (Ord a) => [(Interval a, b)] -> IntervalTree a b
fromList []  = INil
fromList [a] = ILeaf { value = x, interval = i}
    where (i, x) = a
fromList as  = IBranch { value = v, interval = i, left = (fromList leftHalf), right = (fromList rightHalf) }
    where (leftHalf, middle, rightHalf) = split as
          (i, v) = middle

{- Split a list in two sublists and a single `middle' element
 - if the list is split evenly, extracts `middle' element from 
 - the left portion
 -}
split :: [a] -> ([a], a, [a])
split xs = (l, m, r)
    where (l, a, b:bs) = splitMaybe xs
          (m, r) = unwrap a (b:bs)
          unwrap (Just x) ys      = (x, ys)
          unwrap (Nothing) (y:ys) = (y, ys)

{- `Honest' split function, that won't return a middle element if the length
 - of the list is even
 -}
splitMaybe :: [a] -> ([a], Maybe a, [a])
splitMaybe [] = ([], Nothing, [])
splitMaybe [x] = ([], Just x, [])
splitMaybe (x:xs) = (x:ls, m, rs ++ [last xs])
    where (ls, m, rs) = splitMaybe (init xs)

{- Combine two trees together perserving tree balance. -}
combineTrees :: (Ord a) => IntervalTree a b -> IntervalTree a b -> IntervalTree a b 
combineTrees = undefined -- XXX

test1 :: IntervalTree Int Int
test1 = fromList [(Interval (Incl 0) (Excl 1), 10)]

test2 :: IntervalTree Int Int
test2 = fromList [(Interval (Incl 0) (Excl 1), 10), (Interval (Incl 1) (Excl 2), 11)]

test3 :: IntervalTree Int Int
test3 = fromList [(Interval (Incl 0) (Excl 1), 10), (Interval (Incl 1) (Excl 2), 11), (Interval (Incl 2) (Excl 3), 12)]
