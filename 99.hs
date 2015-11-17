-- Find the last element of a list.
myLastElem :: [a] -> a
myLastElem [] = error("Empty list man, get a grip!")
myLastElem [x] = x
myLastElem (_ : xs) = myLastElem xs

-- P2 : Find the last but one element of a list.
myNextLastElem :: [a] -> a
myNextLastElem [] = error("Empty list man, get a grip!")
myNextLastElem [x] = error("There's no next to last element in a 1-element list. Stop messing around.")
myNextLastElem (x : xs) = if length xs > 1
	then myNextLastElem xs
	else x
 
-- P3 : Find the K'th element of a list. The first element in the list is number 1.
myElementAt :: Int -> [x] -> x
myElementAt k [] = error("Empty list, get outta here!")
myElementAt k [x] = if k == 1
	then x
	else error("List is too small, man!")
myElementAt k (x : xs) = if k == 1
	then x
	else myElementAt (k-1) xs

-- P4 : Find the number of elements of a list.
myLength :: [x] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

-- P5 : Reverse a list.
myReverse :: [x] -> [x]
myReverse [] = []
myReverse (x : xs) = (myReverse xs) ++ [x]

-- P6 : Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
myIsPalindrome :: (Eq x) => [x] -> Bool
myIsPalindrome list = list == myReverse list

-- P7 : Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List (x : xs)) = myFlatten x ++ myFlatten (List xs)

-- P8 : Eliminate consecutive duplicates of list elements.
myCompress :: (Eq x) => [x] -> [x]
myCompress [] = []
myCompress [x] = [x]
myCompress (x : xs) = 
	if x == (xs !! 0)
	then myCompress xs
	else [x] ++ myCompress xs
