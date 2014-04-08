module IntervalTree where

{- Primitives for boundaries and intervals -}
data Bound a = Incl a | Excl a
data Interval a = Interval {high :: Bound a, low :: Bound a}

{- Interval tree stores intervals in leafs and branches and allows adding data
 - of any type to each interval. Intervals have to constitute countinuous interval
 - that would span the whole tree
 - Interval tree is a balanced tree.
 -}
data IntervalTree a b = ILeaf {value :: b, interval ::Interval a}
   | IBranch { value :: b, left :: IntervalTree a b, right:: IntervalTree a b}

{- Create a tree consisting of a single interval and a value -}
simpleTree :: (Ord a) => Interval a -> b -> IntervalTree a b
simpleTree int val = ILeaf {value = val, interval = int}

{- Combine two trees together perserving tree balance. -}
combineTrees :: (Ord a) => IntervalTree a b -> IntervalTree a b -> IntervalTree a b 
combineTrees = undefined

