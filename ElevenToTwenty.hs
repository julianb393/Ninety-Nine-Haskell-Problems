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
