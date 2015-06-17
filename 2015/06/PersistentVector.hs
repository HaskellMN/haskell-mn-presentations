
-- an example of a persistent data structure

module PersistentVector where

data Vec = Vec { size :: Int, top :: Node }
  deriving (Show)

data Node = Val Int | Node { left :: Node, pivot :: Int, right :: Node }
  deriving (Show)

vlookup :: Vec -> Int -> Maybe Int
vlookup v x
  | x < 0        = Nothing
  | x > (size v) = Nothing
  | otherwise    = go (top v) x
  where 
    go (Val y) x = Just y
    go (Node l p r) x
      | x < p     = go l x
      | otherwise = go r x

sample = Vec { size = 3, top = nd0 }
  where nd0 = Node nd2      2 (Val 33)
        nd2 = Node (Val 11) 1 (Val 22)

vdump v is = map (vlookup v) is

vmodify :: Vec -> Int -> Int -> Vec
vmodify v x y
  | x > size v  = v  -- array index out of bounds
  | x < 0       = v
  | otherwise   = Vec (size v) (go (top v) x y)
  where
    go (Val _) x y = Val y
    go (Node l p r) x y
      | x < p      = let l' = go l x y in Node l' p r
      | otherwise  = let r' = go r x y in Node l p r'

-- the origin sample vector
test1 = vdump sample [0..2]  -- dump sample[0..2]

-- modify sample[2] = 2000
test2 = let v' = vmodify sample 2 2000 in vdump v' [0..2]

-- modify sample[1] = 1234
test3 = let v' = vmodify sample 1 1234 in vdump v' [0..2]

