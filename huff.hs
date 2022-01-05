{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.String
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

import qualified Data.PQueue.Prio.Min as PQ
import Types
import Storable

import Debug.Trace

instance HuffCodable Char where
  storeTree (Leaf c) = "L" ++ [c]
  storeTree (BinTree l r) = "F" ++ storeTree l ++ storeTree r

main = do

  input <- getContents

  let huff = makeHuff input


  -- print $ tree huff
  putStrLn "Input size:"
  print $ 8 * length input 

  putStrLn "Compressed size (excluding tree):"
  print $ length . unHCode $ mconcat $ map (encode huff) $  map (:[]) input

  putStrLn $ storeHuff huff

  -- let codes = sortOn (length.unHCode) $ map (encode huff) $ map (:[]) $ nub input
  -- print $ take 5 codes

  -- print $ map (decode huff) $ take 5 codes




-- makeHuff :: Ord a => [a] -> Huff a
makeHuff :: [Char] -> Huff Char
makeHuff [] = error "Attempted to create huffman encoder from empty list"
makeHuff as = Huff codemap codetree
  where
    codemap = makeHuffMap Map.empty [] codetree
    codetree = makeHuffTree $ PQ.fromList freqs
    freqs = map (\x -> (length x, Leaf $ head x)) $ group $ sort as 

makeHuffMap ::  Ord a => (Map a HCode) -> [Bool] -> BinTree a -> (Map a HCode)
makeHuffMap m pref (Leaf x)  = Map.insert x (HCode $ reverse pref) m
makeHuffMap m pref (BinTree l r) = addLeft $ addRight m
  where
    addLeft = \m -> makeHuffMap m (False:pref) l
    addRight = \m -> makeHuffMap m (True:pref) r
    

makeHuffTree :: Ord a => PQ Int (BinTree a) -> BinTree a
makeHuffTree pq = case firstTwoPQ pq of
    Nothing -> if null pq 
                then error "Attempted to create huffman encoder from empty list"
                else snd $ PQ.findMin pq
    Just (((k1,v1), (k2,v2)), pq') -> makeHuffTree $ PQ.insert (k1+k2) (BinTree v1 v2) pq'


-- get the first two k/v pairs from the pqueue, or Nothing
--  return the pqueue with elements removed on sucess
firstTwoPQ :: Ord k =>  PQ k v -> Maybe (((k,v), (k,v)), PQ k v)
firstTwoPQ pq = do
  ((k1,v1), pq') <- PQ.minViewWithKey pq
  ((k2,v2), pq'') <- PQ.minViewWithKey pq'
  return (((k1,v1), (k2,v2)), pq'')


encode :: Ord a => Huff a -> [a] -> HCode
encode _ [] = HCode []
-- encode huff@(Huff names _) (a:as) = (names ! a) ++ encode huff as
-- encode huff@(Huff names _) xs = concat $ map (names!) xs
encode huff@(Huff names _) xs = mconcat $ map (\x -> names ! x) xs





-- RECURSIVE
-- Returns as much as it can encode (drops stuff off the end)
decode :: Ord a => Huff a -> HCode -> [a]
decode _ (HCode []) = []
decode huff code = decode' (tree huff) code
  where
    decode' (Leaf a) bs = a:(decode' (tree huff) bs)
    decode' _ (HCode []) = [] -- this may be halfway through a name
    decode' (BinTree l r) (HCode (False:bs)) = decode' l (HCode bs)
    decode' (BinTree l r) (HCode (True:bs)) = decode' r (HCode bs)


-- CPS
-- -- If it can't decode the WHOLE thing, it'll return empty list
-- decode :: Huff a -> HCode -> [a]
-- decode _ (HCode []) = []
-- decode huff code = decode' (tree huff) code id
--   where
--     -- continuation style for the end
--     -- if code string runs out mid-code, it'll call invalid
--     --  otherwise, it'll just return
--     -- decode' :: Ord a => BinTree a -> HCode -> ([a] -> [a]) -> [a]
--     decode' (Leaf x) bs done = decode' (tree huff) bs $ (x:)  
--     decode' _ (HCode []) done = done []
--     decode' (BinTree l r) (HCode (False:bs)) done = decode' l (HCode bs) invalid
--     decode' (BinTree l r) (HCode (True:bs)) done = decode' r (HCode bs) invalid
--     invalid x = []

-- number of leading ^ shows the height of the power tower
--  read the first digit as n
--  for each ^ at the start, n <- read n digits
storeNum :: Int -> String
storeNum n 
  | n < 0 = "-" ++ storeNum (-n)
  | n < 10 = show n
  | otherwise = "^" ++ storeNum (length $ show n) ++ show n


-- -- #TODO generalise to not just Char
-- storeHuff :: Huff Char -> String
-- storeHuff (Huff _ tree) = storeTree tree

-- storeTree :: BinTree Char -> String
-- storeTree (Leaf x) = "1" ++ show x
-- storeTree (BinTree l r) = "0" ++ storeTree l ++ storeTree r

storeHuff :: HuffCodable a => Huff a -> String
storeHuff (Huff _ tree) = storeTree tree


-- storeTree :: Show a => BinTree a -> String
-- storeTree (Leaf x) = "L" ++ storeNum (length $ show x) ++ show x
-- storeTree (BinTree l r) = "F" ++ storeTree l ++ storeTree r

-- storeTree :: Show a => BinTree a -> String
-- storeTree t = trace (show n) $ concat result
--   where
--     result = map ($ padEnd n) stringies
--     (n,stringies) = go 0 t

--     padEnd n [] = take n $ repeat '0'
--     padEnd n (x:xs) = x : padEnd (n-1) xs

--     -- go :: Int -> BinTree a -> (Int, [(String -> String) -> String])
--     go n (Leaf x) = (max n $ length $ show x, [(ignore "L"), (allow $ show x)])
--     go n (BinTree l r) = (n', (ignore "F"):l' ++ r')
--       where 
--         (nl, l') = go n l
--         (n', r') = go nl r


-- allow :: a -> (a -> b) -> b
-- allow x = ($x)

-- ignore :: b -> (a -> b) -> b
-- ignore y = const y