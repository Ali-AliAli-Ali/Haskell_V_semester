import Data.Char

--1
seq1 :: Integer -> Integer
seq1 0 = 1
seq1 1 = 2
seq1 n = 3 * seq1(n-1) - 2 * seq1(n-2) + 1

seq2 :: Integer -> Integer
seq2' :: Integer -> Integer -> Integer -> Integer -> Integer
seq2' n k sk1 sk | k == n  = sk
                 | k < n   = seq2' n (k+1) sk (3*sk - 2*sk1 + 1)
seq2 0 = 1
seq2 1 = 2
seq2 n = seq2' n 1 1 2


--2
isprime :: Integer -> Integer      --check if n is prime
isprime' :: Integer -> Integer -> Integer
isprime n | n == 1     = 0
          | otherwise  = isprime' n 2
isprime' n m | m*m > n       = 1
             | mod n m == 0  = 0 
             | otherwise     = isprime' n (m+1)

degOf2 :: Integer -> Integer        --count nth deg of 2
degOf2 0 = 1
degOf2 n = 2 * degOf2(n-1)

sumDegsOf2 :: Integer -> Integer    --count sum 2^0 + 2^1 + ... + 2^n
sumDegsOf2 n | n < 0      = 0
             | n == 0     = 1
             | otherwise  = sumDegsOf2(n-1) + degOf2(n)

nPerfect :: Integer -> [Integer]    --return array of first n perfect nums
nPerfect' :: Integer -> [Integer] -> Integer -> [Integer]
nPerfect' n perfs t | toInteger(length perfs) == n  = perfs
                    | toInteger(length perfs) < n   = nPerfect' n 
                                                      (perfs ++ (if isprime(sumDegsOf2(t-1)) == 0 
                                                         then [] 
                                                         else [(sumDegsOf2(t-1)) * (degOf2(t-1))])) 
                                                      (t+1)
nPerfect 0 = []
nPerfect n = nPerfect' n [] 1


--3
sumDivrs :: Integer -> Integer
sumDivrs' :: Integer -> Integer -> Integer -> Integer
sumDivrs' n i sum | i*i > n       = sum
                  | mod n i == 0  = sumDivrs' n (i+1) (sum + i + (div n i))
                  | otherwise     = sumDivrs' n (i+1) sum
sumDivrs n = sumDivrs' n 2 1

findTwin :: Integer -> Integer
findTwin n | sumDivrs(sumDivrs n) == n && sumDivrs n /= n  = sumDivrs n
           | otherwise                                       = 0

nTwins :: Integer -> [[Integer]]
nTwins' :: Integer -> [[Integer]] -> [Integer] -> Integer -> [[Integer]]
nTwins' n twins found t | toInteger(length twins) == n  = twins
                        | findTwin t /= 0 && notElem t found && notElem (findTwin t) found
                                                         = nTwins' n (twins ++ [[t, findTwin t]]) 
                                                           (found ++ [t] ++ [findTwin t]) (t+1)
                        | otherwise                      = nTwins' n twins found (t+1)
nTwins 0 = []
nTwins n = nTwins' n [] [] 2


--4
longestStr :: [String] -> String
longestStr' :: [String] -> Int -> String -> String
longestStr' [] maxlen maxstr = maxstr
longestStr' (str:ls) maxlen maxstr | maxlen > length str   = longestStr' ls maxlen maxstr
                                   | otherwise             = longestStr' ls (length str) str
longestStr [] = ""
longestStr ls = longestStr' ls 0 ""


--5
noEvenElts :: [a] -> [a]
noEvenElts' :: [a] -> Integer -> [a]
noEvenElts' [] i = []
noEvenElts' (elt:ls) i | odd i      = elt : noEvenElts' ls (i+1)
                       | otherwise  = noEvenElts' ls (i+1)
noEvenElts ls = noEvenElts' ls 1

noEvenEltsRec :: [[a]] -> [[a]]
noEvenEltsRec ls = noEvenElts(map noEvenElts ls)


--6
hasNum :: String -> Bool
hasNum [] = False
hasNum (letr:str) | isDigit letr  = True
                  | otherwise  = hasNum str

noNumsStrs :: [String] -> [String]
noNumsStrs [] = []
noNumsStrs (str:ls) = (if (hasNum str) then [] else [str]) ++ noNumsStrs(ls)


--7
growSeqs :: [Integer] -> [[Integer]]
growSeqs' :: [Integer] -> [Integer] -> [[Integer]]
growSeqs' [] seq = [seq]
growSeqs' (x:ls) seq | null seq || last seq < x  = growSeqs' ls (seq ++ [x])
                     | otherwise                  = seq : growSeqs' ls [x]
growSeqs ls = growSeqs' ls []


--8
factl :: Integer -> Integer
factl 0 = 1
factl n = n * factl(n-1)

countUn :: Double -> Integer -> Double
countUn x n = x^n / (fromIntegral(factl n) + 1)

scarySum :: Double -> [Double]
scarySum' :: Double -> Integer -> [Double] -> [Double]
scarySum' x n [sum, step] 
                  | abs(countUn x n) < abs(x)  = [sum, fromIntegral n]
                  | otherwise                  = scarySum' x (n+1) [sum + countUn x n, fromIntegral(n+1)]
scarySum x = scarySum' x 2 [x,2]


main = do
{-    
    --1 to correct
    print(seq2 1, seq2 2, seq2 3, seq2 4, seq2 5, seq2 10, seq2 50)
    print(seq1 1, seq1 2, seq1 3, seq1 4, seq1 5, seq1 10)
    --2
    print(nPerfect 0, nPerfect 1, nPerfect 3, nPerfect 4, nPerfect 8)
    --3
    print(nTwins 6)
    --4
    print(longestStr ["62", "Ali4", "Abracada~bra14", "Yeee5", "CadabraPokemon16"], 
          longestStr [""], longestStr ["42"])
    --5
    print(noEvenEltsRec ["Twins", "are", "pair", "of", "natural", "numbers"])
    --6
    print(noNumsStrs ["62", "Ali4", " Abracada~bra ", "Yeee5", "CadabraPokemon"])
    --7
    print(growSeqs [1,2,3,4,7,5,4,3,6,8,5])
    --8
    print(scarySum 2, scarySum 4, scarySum 10)
-}  
    