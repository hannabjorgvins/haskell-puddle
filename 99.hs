import Data.List

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
	else x : myCompress xs

-- P9 : Pack consecutive duplicates of list elements into sublists.
myPack :: (Eq x) => [x] -> [[x]]
myPack [] = [[]]
myPack [x] = [[x]]
myPack (x : xs) = if x `elem` (head (myPack xs))
	then (x : (head (myPack xs))) : tail (myPack xs)
	else [x] : myPack xs


-- P10 : Run-length encoding of a list.
myEncode :: (Eq y) => [y] -> [(Int,y)]
myEncode [] = []
myEncode list = (length x, head x) : myEncode (dropWhile (== head x) list)
	where
		(x : xs) = myPack list

-- P11 : Modified run-length encoding.
data MS a = Single a | Multiple Int a deriving Show

myModifiedEncode :: (Eq a) => [a] -> [MS a]
myModifiedEncode [] = []
myModifiedEncode list = map (\x -> value (length x) (head x)) (myPack list)
        where
		value 1 h = Single h
                value n h =  Multiple n h

-- P12 : Decode a run-length encoded list.
myDecode :: (Eq a) => [MS a] -> [a]
myDecode [] = []
myDecode (x:xs) = decodeFirst x ++ myDecode xs
	where
		decodeFirst (Single a) = [a]
		decodeFirst (Multiple n a) = replicate n a

-- P13 : Run-length encoding of a list (direct solution).
myDirectEncode :: (Eq a) => [a] -> [MS a]
myDirectEncode [] = []
myDirectEncode [x] = [Single x]
myDirectEncode (x:xs) = process x (myDirectEncode xs)  
	where
		process x ((Single h) : rest) = if x == h 
						then Multiple 2 x : rest 
						else Single x : Single h : rest
		process x ((Multiple n h) : rest) = if x == h 
						then Multiple (n+1) x : rest 
						else Single x : Multiple n h : rest

-- P14: Duplicate the elements of a list.
myDuplicate :: [a] -> [a]
myDuplicate [] = []
myDuplicate (x:xs) = x : x : myDuplicate xs

-- P15: Replicate the elements of a list a given number of times.
myReplicate :: [b] -> Int -> [b]
myReplicate [] _ = []
myReplicate _ 0 = []
myReplicate (x:xs) n = replicate n x ++ myReplicate xs n  

-- P16: Drop every N'th element from a list.
myDropEvery :: [a] -> Int -> [a]
myDropEvery [] _ = []
myDropEvery list 0 = list
myDropEvery list n = if length list < n then list else take (n-1) list ++ myDropEvery (drop n list) n 

-- P17: Split a list into two parts; the length of the first part is given.
mySplit :: [a] -> Int -> ([a], [a])
mySplit list 0 = ([], list)
mySplit list n = if length list < n then (list, []) else (take n list, drop n list)

-- P18: Extract a slice from a list.
mySlice :: [a] -> Int -> Int -> [a]
mySlice [] _ _ = []
mySlice list from to = if from > to then [] else take (to - from + 1) $ drop (from - 1) list

-- P19: Rotate a list N places to the left.
myRotate :: [a] -> Int -> [a]
myRotate [] _ = []
myRotate list n
	| n == 0 = list
	| n > 0 = drop n list ++ take n list
	| otherwise = drop l list ++ take l list
		where l = length list + n 

-- P20: Remove the K'th element from a list.
myDropAt :: Int -> [a] -> [a]
myDropAt _ [] = []
myDropAt n list 
	| (n < 1) || (n > length list) = list
	| n == 1 = drop 1 list
	| otherwise = take (n-1) list ++ drop n list
