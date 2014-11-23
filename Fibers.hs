module Fibers where

import Prelude hiding (lookup)
import Data.Map (Map, insert, lookup, fromList, toList)
import Data.List (foldl')

-- `fibers` refers to set of preimages of a set theoretic map f : A -> B, see
-- http://en.wikipedia.org/wiki/Fiber_%28mathematics%29
-- inspired by Clojure's group-by (and distinct from Haskell's groupBy) see
-- https://github.com/clojure/clojure/blob/clojure-1.6.0/src/clj/clojure/core.clj#L6590

fibers :: Ord b => (a -> b) -> [a] -> Map b [a]
fibers f xs = foldl' (\acc x -> let y = f x in
    insert y (case lookup y acc of Nothing -> [x]
                                   Just ys -> ys ++ [x]) acc)
    (fromList []) xs

{-
>> fibers odd [0..9]
fromList [(False,[0,2,4,6,8]),(True,[1,3,5,7,9])]

>>> fibers (\x -> x*x) [-5..5]
fromList [(0,[0]),(1,[-1,1]),(4,[-2,2]),(9,[-3,3]),(16,[-4,4]),(25,[-5,5])]
-}
