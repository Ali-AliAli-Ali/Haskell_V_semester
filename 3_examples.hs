data Name = Ali | Artem | Slava

hoursWorking :: Name -> Integer
hoursWorking Ali   = 20
hoursWorking Artem = 0
hoursWorking Slava = 40

isStudent :: Name -> Bool
isStudent Slava = False
isStudent _     = True


data Point3D = Point3D Double Double Double

dist :: Point3D -> Point3D -> Double
dist (Point3D x1 y1 z1) (Point3D x2 y2 z2) = sqrt((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)


data Pair a = Pair a a

--printPair :: Pair a -> IO ()
--printPair (Pair x y) = print(x,y)

first :: Pair a -> a
first (Pair x y) = x

second :: Pair a -> a
second (Pair x y) = y

addPair :: Pair Integer -> Pair Integer -> Pair Integer
addPair (Pair x y) (Pair u v) = Pair (x+u) (y+v)

swap :: Ord a => Pair a -> Pair a
swap (Pair x y) = Pair y x


data IntList = EmpList | SomeList IntList Integer

maxElt :: IntList -> Integer
maxElt EmpList              = 0
maxElt (SomeList EmpList x) = x
maxElt (SomeList ls x)      = max x (maxElt ls)


data AList a = EmpAList | SomeAList (AList a) a

maxAElt :: Ord a => AList a -> a
maxAElt EmpAList               = error "maxAElt: Empty list"
maxAElt (SomeAList EmpAList x) = x
maxAElt (SomeAList ls x)       = max x (maxAElt ls)


onesList :: [Integer]
onesList = 1:onesList

numsList :: [Integer]
numsList' :: Integer -> [Integer]
numsList' n = n : numsList' (n+1)
numsList = numsList' 0

summ :: Integer -> [Integer] -> Integer
summ 0 _ = 0
summ n (x:ls) = x + (summ (n-1) ls) 


sieveErath :: [Integer] -> [Integer]
sieveErath [] = []
sieveErath (n:ls) = n : sieveErath [x | x <- ls, mod x n /= 0]


fib :: [Integer]
fib = 1 : zipWith (+) fib (0 : fib) 


--exercises
--1
sqrDiff1 :: Integer -> Integer -> [Integer]
sqrDiff1 a b = zipWith (-) [x^2 | x <- [a..b]] [x^2 | x <- [(a-1)..(b-1)]]

sqrDiff2 :: Integer -> Integer -> [Integer]
sqrDiff2 a b = zipWith (-) sqrs ((a-1)^2:sqrs) 
               where sqrs = [x^2 | x <- [a..b]]

--2
primeList :: [Integer]
primeList = sieveErath [2..]

simpleTwins1 :: [[Integer]]
simpleTwins1' :: [Integer] -> [[Integer]] -> Integer -> [[Integer]]
simpleTwins1' [] _ _        = []
simpleTwins1' (x:ls) res 50 = res
simpleTwins1' (x:ls) res k  = if ((head ls) - x == 2) 
                                 then (simpleTwins1' ls (res ++ [[x, head ls]]) (k+1)) 
                                 else (simpleTwins1' ls res k)
simpleTwins1 = simpleTwins1' (primeList) [] 0


pairs :: [Integer] -> [(Integer, Integer)]
pairs (x:s@(y:_)) = (x,y) : pairs s 

simpleTwins2 :: [(Integer, Integer)]
simpleTwins2 = [(x,y) | (x,y) <- pairs primeList, y-x == 2] 

--3
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists ls1@(x:tail1) ls2@(y:tail2) = if (x <= y) then x:(mergeLists tail1 ls2)
                                                     else y:(mergeLists ls1 tail2)
powers2 = iterate (2*) 2 
powers3 = iterate (3*) 6 

nthPowers23 :: Integer -> Integer
nthPowers23 n = (mergeLists powers2 powers3) !! fromIntegral(n)

--5
diagMatr :: [[Integer]] 
diagMatr = iterate (map (+1) . tail) str1
           where str1 = 1 : zipWith (+) str1 [1..]

sumFstNofJthCol :: [[Integer]] -> Integer -> Integer -> Integer
sumFstNofJthCol _ 0 _ = 0
sumFstNofJthCol (row:matr) n j = (row !! fromIntegral(j)) + (sumFstNofJthCol matr (n-1) j)

sumNJDiagMatr :: Integer -> Integer -> Integer
sumNJDiagMatr n j = sumFstNofJthCol diagMatr n j

main = do
{-
    print(hoursWorking Artem, hoursWorking Ali, isStudent Ali, isStudent Slava)
    print(dist (Point3D 1 1 1) (Point3D 1 0 0))
    print(first (Pair 3 4), first (Pair "Ali" "Yey"), second (Pair True False))
    print(first (addPair (Pair 3 4) (Pair 10 20)), second (addPair (Pair 3 4) (Pair 10 20)))

    print(maxElt (SomeList (SomeList (SomeList EmpList 5) 8) 3))
    print(maxAElt (SomeAList (SomeAList (SomeAList EmpAList 5) 8) 3))
    print(maxAElt (SomeAList (SomeAList (SomeAList EmpAList 5.0) 8.7) 3.2))
    print(maxAElt (SomeAList (SomeAList (SomeAList EmpAList 'a') 'l') 'i'))
    print(maxAElt (SomeAList (SomeAList (SomeAList EmpAList "Ali") "Slava") "Artem"))

    print(summ 5 onesList)
    print(summ 5 numsList)
    print(summ 5 [x^2 | x <- [1..]])
    print(sieveErath [2..1000])

    print(summ 10 (repeat 1), summ 8 (cycle [1..4]), summ 5 (iterate (*2) 1))
    print(fib !! 0, fib !! 2, fib !! 3, fib !! 4, fib !! 5, fib !! 100)
    print(zipWith (^) (repeat 2) [1..5], zipWith (^) [1..5] (repeat 2))

    print(sqrDiff1 1 10, sqrDiff1 10 20, sqrDiff1 10 10)
    print(sqrDiff2 1 10, sqrDiff2 10 20, sqrDiff2 10 10)
    print(simpleTwins1)
    print(take 50 simpleTwins2)
-}
    print(nthPowers23 0, nthPowers23 1, nthPowers23 2, nthPowers23 3, nthPowers23 4, nthPowers23 20)
    print(sumNJDiagMatr 10 5, sumNJDiagMatr 3 3, sumNJDiagMatr 4 2)
    