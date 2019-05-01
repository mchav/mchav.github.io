---
layout: post
title: Union find
published: true
---

### Motivation
Let's take on the role of a seasoned social media stalker. To make it more tasteful, let's call it "networking." We'd like to find out who's who on the internet and also their relation to us or the people we know. That is, given a person of interest we'd like to know if it's possible to get in touch with them through a friend or a friend of a friend and so on. This is called the connectivity problem. It asks if there is some connection between two objects. And to solve it we need to design a data structure that supports two fundamental operations between the objects of concern: connecting two objects and finding out if two objects are connected. Unsurprisngly, the data structure that solves this problem is called union-find.

### Implementing Union Find
Let's simplify the problem from talking about random objects to talking about integers. Given n objects we represent them with integers 1 to n.

```haskell
import Control.Monad
import Data.IORef
import Data.List hiding (union, find)
import Data.Maybe

data UnionFind = UnionFind { ids :: [Int] } deriving (Eq, Show)

empty :: Int -> UnionFind
empty n = UnionFind { ids = [0..(n-1)] }

find :: UnionFind -> Int -> Int
find uf p = fromJust (lookup p (zip [0..] (ids uf)))

union :: UnionFind -> Int -> Int -> UnionFind
union uf p q = UnionFind { ids = updated }
  where rootP = find uf p
        rootQ = find uf q
        updated = map (\x -> if x == rootQ then rootP else x) (ids uf)

connected :: UnionFind -> Int -> Int -> Bool
connected uf p q = (find uf p) == (find uf q)

count :: UnionFind -> Int
count = length . group . sort . ids

-- Social Network Connectivity
type Friendship = (Int, Int, Int)

time :: Friendship -> Int
time (_, _, t) = t

people :: Friendship -> (Int, Int)
people (p, q, _) = (p, q)

social :: UnionFind -> [(Int, Int, Int)] -> Int
social uf []     = (-1)
social uf (f:fs)
  | components == 1 = time f
  | otherwise = social newUnion fs
    where newUnion = (uncurry (union uf)) (people f)
          components = count newUnion

-- union find with canonical element

-- successor with delete

main = do
  let s = (empty 10)
  let friendships = zip3 [0..9] (cycle [1]) [5..14]
  print (social s friendships)
  uf' <- newIORef (empty 10)
  forM_ [0..8] $ \i -> do
    uf <- readIORef uf'
    let newUnion = union uf i 1
    print newUnion
    writeIORef uf' newUnion


```
