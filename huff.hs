import Data.List
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map



main = interact $ 
  show . solve

solve :: String -> [String]
solve xs = [show $ tree huff]
  -- [decode huff codes] ++ (map writeCode $ [encode huff "a", encode huff "b", encode huff "abba"])
  where
    -- codes = map (encode huff . pure) vals
    codes = encode huff vals
    huff = makeHuff xs
    vals = unique xs


-- -- doing insertion sort
-- unique :: Ord a => [a] -> [a]
-- unique xs = unique' [] xs
--   where
--     unique' vs [] = vs
--     unique' vs (x:xs) = unique' (insertUnique x vs) xs
--     insertUnique x [] = [x]
--     insertUnique x (y:ys) = case compare x y of
--       LT -> x:y:ys
--       EQ -> y:ys
--       GT -> y:insertUnique x ys

-- turns out that's built into Data.List
unique = nub




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
type HCode = [Bool]


writeCode :: HCode -> String
writeCode [] = ""
writeCode (False:xs) = "0" ++ writeCode xs
writeCode (True:xs) = "1" ++ writeCode xs



-- makeHuff :: Ord a => [a] -> Huff a
makeHuff :: [Char] -> Huff Char
makeHuff as = Huff codemap codetree
  where
    codetree = maketree freqs
    -- codetree = maketree [(Leaf 'b',2), (Leaf 'a',5)]

    -- RECURSIVE STYLE
    codemap = makemap Map.empty [] codetree

    makemap ::  Ord a => (Map.Map a HCode) -> HCode -> BinTree a -> (Map.Map a HCode)
    makemap m pref (Leaf x)  = Map.insert x pref m
    makemap m pref (BinTree l r)  = makemap m' (pref++[True]) r
      where
        m' = makemap m (pref++[False]) l

    -- --  CONTINUATION STYLE
    -- codemap = makemap Map.empty [] codetree id
    -- makemap ::  Ord a => (Map.Map a HCode) -> HCode -> BinTree a -> (Map.Map a HCode->Map.Map a HCode) -> (Map.Map a HCode)
    -- makemap m pref (Leaf x) ret = ret $ Map.insert x pref m
    -- makemap m pref (BinTree l r) ret = makemap m (pref++[False]) l doRight
    --   where
    --     doRight = \m -> makemap m (pref++[True]) r ret

    maketree :: Ord a => [(BinTree a,Int)] -> BinTree a
    maketree [] = error "Attempted to create huffman encoder from empty list"
    maketree [x] = fst x
    maketree [(x,f1),(y,f2)] = BinTree x y
    maketree ((x1,f1) : (x2,f2) : xs) = maketree (insertBy comp2 (BinTree x1 x2, f1+f2) xs) 

    comp2 = (\(_,f1) (_,f2) -> compare f1 f2)

    freqs = sortBy comp2 $ map (\x -> (Leaf $ head x, length x)) $ group as


encode :: Ord a => Huff a -> [a] -> HCode
encode _ [] = []
encode huff@(Huff names _) (a:as) = (names ! a) ++ encode huff as

-- decode :: Ord a => Huff a -> HCode -> [a]
-- decode _ [] = []
-- decode huff code = decode' (tree huff) code
--   where
--     decode' _ [] = [] -- this may be halfway through a name
--     decode' (Leaf a) bs = a:(decode' (tree huff) bs)
--     decode' (BinTree l r) (False:bs) = decode' l bs
--     decode' (BinTree l r) (True:bs) = decode' r bs



-- follow the HCode down the Tree (False for Left, True for right)
--  returns only as much as it can decode, the rest is dropped.  
decode :: Ord a => Huff a -> HCode -> [a]
decode _ [] = []
decode huff code = decode' (tree huff) code id
  where
    -- continuation style for the end
    -- if code string runs out mid-code, it'll call invalid
    --  otherwise, it'll just return
    -- decode' :: Ord a => BinTree a -> HCode -> ([a] -> [a]) -> [a]
    decode' _ [] done = done []
    decode' (Leaf x) bs done = x:(decode' (tree huff) bs id)
    decode' (BinTree l r) (False:bs) done = decode' l bs invalid
    decode' (BinTree l r) (True:bs) done = decode' r bs invalid
    invalid x = []


