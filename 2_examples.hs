import Data.Char

--  examples
listSum1 :: [Integer] -> Integer
listSum1 l | null l     = 0
           | otherwise  = head l + listSum1(tail l)

listSum2 :: [Integer] -> Integer
listSum2 [] = 0
listSum2 (x:l) = x + listSum2 l


listToString1 :: [Integer] -> String
listToString1 l | length l == 1  = show(head l) ++ ""
                | otherwise      = show(head l) ++ listToString1(tail l)

listToString2 :: [Integer] -> String
listToString2 l | length l == 1  = show(head l) ++ ""
listToString2 (x:l) = show(x) ++ listToString2(l)

listToString3 :: [Bool] -> String
listToString3 l | length l == 1  = show(head l) ++ ""
listToString3 (x:l) = show(x) ++ listToString3(l)


reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:l) = reverse1(l) ++ [x]

reverse2 :: [a] -> [a]
reverse2' :: [a] -> [a] -> [a]
reverse2 s = reverse2' [] s
reverse2' s [] = s
reverse2' s1 (x:s2) = reverse2' (x:s1) s2 


isStrWhite :: String -> Bool
isStrWhite [] = True
isStrWhite (x:s) | isSpace x  = isStrWhite s
                 | otherwise  = False
hasWhites :: [String] -> Bool
hasWhites [] = False
hasWhites (x:s) | isStrWhite x  = True
                | otherwise     = hasWhites s


--  exercises
--1
hasLetter :: String -> Integer
hasLetter [] = 0
hasLetter (x:s) | isAlpha x  = 1
                | otherwise  = hasLetter s
nonEmptyStrs :: [String] -> Integer
nonEmptyStrs [] = 0
nonEmptyStrs (str:l) = (hasLetter str) + (nonEmptyStrs l)

test_nonEmptyStrs:: [[String]] -> [Integer]
test_nonEmptyStrs [] = []
test_nonEmptyStrs (arr:l) = [nonEmptyStrs arr] ++ (test_nonEmptyStrs l)


main = do
{-    
    print("Ali" == ['A', 'l', 'i'], [1..10], [2,5..14], [2,5..15])
    print([1,2,3] ++ [8,8], 4:6:8 : [1,2,3], 2:[])

    print("The list", [1..4])
    print("Head", head [1..4])
    print("Last", last [1..4])
    print("Tail", tail [1..4])
    print("Init", init [1..4])
    print("1st element", [1..4] !! 1)

    print(listSum1 [1..5])
    print(listSum2 [1..5])
    print(listToString1 [1..5])
    print(listToString2 [5,4..1])
    print(listToString3 [True, False, True, True])
    print(reverse1 [5,4..1], reverse1 "AliYey")
    print(reverse2 [5,4..1], reverse2 "AliYey")
    print(hasWhites ["Ali", "", "Yyy"], hasWhites [], hasWhites ["Aa", "ll", "ii"], hasWhites ["Aaa", " "])

    print(test_nonEmptyStrs [["Hello, world!", "", "12345", "1-a"], ["*", "1+2", "", "17"], [], ["Volga", "Neva", "Neman", "X17"]])

