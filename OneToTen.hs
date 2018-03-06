-- @author: Julian Barker
-- My own solutions to Problem 1 - 10. Please note that there
-- are various ways of implementing these functions.

module OneToTen where

-- PROBLEM 1 Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "This list is empty!"
myLast [x] = x
myLast (x:xs) = myLast xs

-- PROBLEM 2 Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "This list is empty!"
myButLast [x] = error "This list only has one element!"
myButLast [x,_] = x
myButLast (x:xs) = myButLast xs

-- Problem 3 Return the kth element of a list.
elementAt :: [a] -> Int -> a
elementAt lst i
  |length lst < i = error "Index out of range."
  |otherwise = lst !! (i - 1)

-- Problem 4 Return length of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

-- Problem 5
myReverse :: [a] -> [a]
myReverse lst = foldr op [] lst
  where op = (\x acc -> acc ++ [x])

-- Problem 6 Returns whether a string is a palindrome or not.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs)
  |x == (last xs) = isPalindrome (init xs)
  |otherwise = False

-- Problem 7 Flatten a nested list.
-- Define a nested data.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- Problem 8 Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (==x) xs)

-- Problem 9  Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack lst = [before] ++ pack (drop (length before) lst)
  where before = takeWhile (== (head lst)) lst

-- Problem 10 Run-length encoding of a list.
-- Prerequisite: Problem 9.
encode :: (Eq a) => [a] -> [(Int, a)]
encode lst = map convTup packedLst
  where packedLst = pack lst
        convTup = (\lst -> (length lst, head lst))
