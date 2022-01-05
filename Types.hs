module Types where

import Data.String

import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

import qualified Data.PQueue.Prio.Min as PQ

type PQ = PQ.MinPQueue
type Map = Map.Map




data Huff a = Huff
  { names :: Map a HCode
  , tree :: BinTree a
  }
-- could be invalid (eg if tree only has a leaf)


data BinTree a = Leaf a | BinTree (BinTree a) (BinTree a)

instance Functor BinTree where
  fmap f (Leaf x) = Leaf $ f x
  fmap f (BinTree l r) = BinTree (fmap f l) (fmap f r)
instance Show a => Show (BinTree a) where
  show (Leaf x) = show x
  show (BinTree l r) = show (l, r)

newtype HCode = HCode { unHCode :: [Bool] }
instance Semigroup HCode where
  (HCode a) <> (HCode b) = HCode $ a <> b
instance Monoid HCode where
  mempty = HCode []
instance IsString HCode where
  fromString xs = HCode $ map (\x -> if x == '0' then False else True) xs

instance Show HCode where
  show (HCode xs) = map binShow xs

binShow :: Bool -> Char
binShow False = '0'
binShow True = '1'