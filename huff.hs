{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.String
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Hashable

import qualified Data.PQueue.Prio.Min as PQ

import Debug.Trace


type PQ = PQ.MinPQueue
type Map = Map.Map

main = do
  input <- getContents

  let huff = makeHuff input
  -- print $ tree huff

  let codes = sortOn (length.unHCode) $ map (encode huff) $ map (:[]) $ nub input
  print $ take 5 codes

  print $ map (decode huff) $ take 5 codes
  return ()



-- binary codes (left/right only)

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

-- makeHuff :: Ord a => [a] -> Huff a
makeHuff :: [Char] -> Huff Char
makeHuff as = Huff codemap codetree
  where
    -- codetree = maketree [(Leaf 'b',2), (Leaf 'a',5)]

    -- RECURSIVE STYLE
    codemap = makemap Map.empty [] codetree
    makemap ::  Ord a => (Map a HCode) -> [Bool] -> BinTree a -> (Map a HCode)
    makemap m pref (Leaf x)  = Map.insert x (HCode $ reverse pref) m
    makemap m pref (BinTree l r) = addLeft $ addRight m
      where
        addLeft = \m -> makemap m (False:pref) l
        addRight = \m -> makemap m (True:pref) r

    -- --  CONTINUATION STYLE 
    -- WAY SLOWER, HAS SPACE LEAKS
    -- codemap = makemap Map.empty [] codetree id
    -- makemap ::  (Hashable a, Ord a) => (Map a HCode) -> [Bool] -> BinTree a -> (Map a HCode->Map a HCode) -> (Map a HCode)
    -- makemap m pref (Leaf x) ret = ret $ Map.insert x (HCode $ reverse pref) m
    -- makemap m pref (BinTree l r) ret = makemap m (False:pref) l $ ret . addRight
    --   where
    --     addRight = \m -> makemap m (True:pref) r ret

    -- codetree = maketree freqs
    -- -- freqs :: [(BinTree a, Int)]
    -- freqs = sortOn snd $ map (\x -> (Leaf $ head x, length x)) $ group $ sort as 
    -- comp2 (_,x1) (_,x2) = compare x1 x2
    -- maketree :: Ord a => [(BinTree a,Int)] -> BinTree a
    -- maketree [] = error "Attempted to create huffman encoder from empty list"
    -- maketree [x] = fst x
    -- maketree [(x,f1),(y,f2)] = BinTree x y
    -- maketree ((x1,f1) : (x2,f2) : xs) = maketree (insertBy comp2 (BinTree x1 x2, f1+f2) xs) 





    -- Instead of using insertBy, use a priorityQueue  
    codetree = maketree $ PQ.fromList freqs
    freqs = map (\x -> (length x, Leaf $ head x)) $ group $ sort as  
    maketree :: Ord a => PQ Int (BinTree a) -> BinTree a
    maketree pq = case firstTwoPQ pq of
      Nothing -> if null pq 
                  then error "Attempted to create huffman encoder from empty list"
                  else snd $ PQ.findMin pq
      Just (((k1,v1), (k2,v2)), pq') -> maketree $ PQ.insert (k1+k2) (BinTree v1 v2) pq'




firstTwoPQ :: Ord k =>  PQ k v -> Maybe (((k,v), (k,v)), PQ k v)
firstTwoPQ pq = do
  ((k1,v1), pq') <- PQ.minViewWithKey pq
  ((k2,v2), pq'') <- PQ.minViewWithKey pq'
  return (((k1,v1), (k2,v2)), pq'')


encode :: (Hashable a, Ord a) => Huff a -> [a] -> HCode
encode _ [] = HCode []
-- encode huff@(Huff names _) (a:as) = (names ! a) ++ encode huff as
-- encode huff@(Huff names _) xs = concat $ map (names!) xs
encode huff@(Huff names _) xs = mconcat $ map (\x -> names ! x) xs

-- decode :: Ord a => Huff a -> HCode -> [a]
-- decode _ [] = []
-- decode huff code = decode' (tree huff) code
--   where
--     decode' _ [] = [] -- this may be halfway through a name
--     decode' (Leaf a) bs = a:(decode' (tree huff) bs)
--     decode' (BinTree l r) (False:bs) = decode' l bs
--     decode' (BinTree l r) (True:bs) = decode' r bs



-- follow the HCode down the Tree (False for Left, True for right)
-- If it can't decode the WHOLE thing, it'll return empty list
-- decode :: Ord a => Huff a -> HCode -> [a]
decode :: Huff a -> HCode -> [a]
decode _ (HCode []) = []
decode huff code = decode' (tree huff) code id
  where
    -- continuation style for the end
    -- if code string runs out mid-code, it'll call invalid
    --  otherwise, it'll just return
    -- decode' :: Ord a => BinTree a -> HCode -> ([a] -> [a]) -> [a]
    decode' (Leaf x) bs done = decode' (tree huff) bs $ (x:)  
    decode' _ (HCode []) done = done []
    decode' (BinTree l r) (HCode (False:bs)) done = decode' l (HCode bs) invalid
    decode' (BinTree l r) (HCode (True:bs)) done = decode' r (HCode bs) invalid
    invalid x = []


