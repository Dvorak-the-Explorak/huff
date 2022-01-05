{-# LANGUAGE DefaultSignatures #-}

module Storable where
import Types


class HuffCodable a where
  storeTree :: BinTree a -> String

  default storeTree :: Show a => BinTree a -> String
  storeTree t = concat result
    where
      result = map ($ padEnd n) stringies
      (n,stringies) = go 0 t

      padEnd n [] = take n $ repeat '0'
      padEnd n (x:xs) = x : padEnd (n-1) xs

      -- go :: Int -> BinTree a -> (Int, [(String -> String) -> String])
      go n (Leaf x) = (max n $ length $ show x, [(ignore "L"), (allow $ show x)])
      go n (BinTree l r) = (n', (ignore "F"):l' ++ r')
        where 
          (nl, l') = go n l
          (n', r') = go nl r


allow :: a -> (a -> b) -> b
allow x = ($x)

ignore :: b -> (a -> b) -> b
ignore y = const y