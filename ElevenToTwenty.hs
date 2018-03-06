-- @author: Julian Barker
-- My own solutions to Problem 11 - 20. Please note that there
-- are various ways of implementing these functions.

module ElevenToTwenty where

import OneToTen

data Quantity a b = Single b | Multiple a b
  deriving Show

-- Problem 11 Modified run-length encoding.
encodeModified :: (Eq a) => [a] -> [Quantity Int a]
encodeModified lst = map encodePiece pLst
  where pLst = pack lst
        encodePiece piece
          |length piece == 1 = Single (head piece)
          |otherwise = Multiple (length piece) (head piece)

-- Problem 12 Decode a run-length encoded list.
decodeModified :: [Quantity Int a] -> [a]
decodeModified [] = []
decodeModified (y:ys) = (decodePiece y) ++ (decodeModified ys)
  where decodePiece (Single x) = [x]
        decodePiece (Multiple times x) = replicate times x

-- Problem 13 Run-length encoding of a list (direct solution).
encodeDirect :: (Eq a) => [a] -> [Quantity Int a]
encodeDirect [] = []
encodeDirect lst@(x:xs) = let r = (takeW lst) in
  [encoded r] ++ encodeDirect (drop (length r) lst)
    where encoded elst
            |length elst == 1 = Single (head elst)
            |otherwise = Multiple (length elst) (head elst)
          takeW (x:xs) = takeWhile (== x) (x:xs)

-- Problem 14 Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x, x] ++ (dupli xs)

-- Problem 15 Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) i = (replicate i x) ++ (repli xs i)

-- Problem 16 Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery lst i
  |(length lst) < i = lst
  |otherwise = (init $ take i lst) ++ (dropEvery (drop i lst) i)
  -- For otherwise condition, could have just done take i-1 ...

-- Problem 17 Split a list into two parts;
-- the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split lst i = (take i lst, drop i lst)

-- Problem 18 Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice lst i k = take (k - i + 1) $ drop (i - 1) lst

-- Problem 19 Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate lst i
  |i < 0 = (drop ((length lst) + i) lst) ++ (take ((length lst) + i) lst)
  |i == 0 = lst
  |otherwise = (drop i lst) ++ (take i lst)

-- Problem 20 Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt i lst = case back of
  [] -> error "Index out of bounds."
  (x:xs) -> (x, front ++ xs)
  where (front, back) = splitAt (i - 1) lst
