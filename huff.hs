{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.String
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

import Debug.Trace

main = do
  input <- getContents

  let huff = makeHuff input
  -- print $ tree huff

  let codes = map (encode huff) $ map (:[]) $ take 5 $ nub input
  print codes

  print $ map (decode huff) codes
  return ()



-- binary codes (left/right only)

data Huff a = Huff
  { names :: Map.Map a HCode
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

newtype HCode = HCode [Bool]
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
    codetree = maketree freqs
    -- codetree = maketree [(Leaf 'b',2), (Leaf 'a',5)]

    -- RECURSIVE STYLE
    -- codemap = makemap Map.empty [] codetree
    -- makemap ::  Ord a => (Map.Map a HCode) -> [Bool] -> BinTree a -> (Map.Map a HCode)
    -- makemap m pref (Leaf x)  = Map.insert x (HCode $ reverse pref) m
    -- makemap m pref (BinTree l r) = addLeft $ addRight m
    --   where
    --     addLeft = \m -> makemap m (False:pref) l
    --     addRight = \m -> makemap m (True:pref) r

    --  CONTINUATION STYLE
    codemap = makemap Map.empty [] codetree id
    makemap ::  Ord a => (Map.Map a HCode) -> [Bool] -> BinTree a -> (Map.Map a HCode->Map.Map a HCode) -> (Map.Map a HCode)
    makemap m pref (Leaf x) ret = ret $ Map.insert x (HCode $ reverse pref) m
    makemap m pref (BinTree l r) ret = makemap m (False:pref) l $ ret . addRight
      where
        addRight = \m -> makemap m (True:pref) r ret


    -- Instead of using insertBy, use a priorityQueue
    maketree :: Ord a => [(BinTree a,Int)] -> BinTree a
    maketree [] = error "Attempted to create huffman encoder from empty list"
    maketree [x] = fst x
    maketree [(x,f1),(y,f2)] = BinTree x y
    maketree ((x1,f1) : (x2,f2) : xs) = maketree (insertBy comp2 (BinTree x1 x2, f1+f2) xs) 

    comp2 = (\(_,f1) (_,f2) -> compare f1 f2)
    -- freqs = sortBy comp2 $ map (\x -> (Leaf $ head x, length x)) $ group as

    freqs = sortOn snd $ map (\x -> (Leaf $ head x, length x)) $ group as


encode :: Ord a => Huff a -> [a] -> HCode
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


