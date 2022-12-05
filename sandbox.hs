twice :: Integer -> Integer
twice x = 2*x


factorial1 :: Integer -> Integer
factorial1 n = 
    if n == 0 then 1 
    else n*factorial1(n-1)

factorial2 :: Integer -> Integer
factorial2 n | n == 0  = 1
             | n > 0  = n * factorial2(n-1)

factorial3 :: Integer -> Integer
factorial3 0 = 1
factorial3 n = n * factorial3(n-1)


gcd1 :: Integer -> Integer -> Integer
gcd1 n m | n < 0 || m < 0           = error "gcd1: Negative arguments"
         | m == 0                   = n 
         | n == 0                   = m
         | n > 0 && m > 0 && n > m  = gcd1 m (mod n m)
         | n > 0 && m > 0 && n < m  = gcd1 n (mod m n)

gcd2 :: Integer -> Integer -> Integer
gcd2 m n | m < 0 = error "gcd2: Negative 1st argument"
         | n < 0 = error "gcd2: Negative 2nd argument"
gcd2 m 0 = m
gcd2 m n = gcd2 n (m `mod` n) 


isprime :: Integer -> Bool
isprime' :: Integer -> Integer -> Bool
isprime n | n < 0      = error "isprime: Negative argument"
          | n <= 1     = error "isprime: Neither prime nor composite number"
          | otherwise  = isprime' n 2
isprime' n m | m*m > n       = True
             | mod n m == 0  = False 
             | otherwise     = isprime' n (m+1)


fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib k = fib(k-1) + fib(k-2)

ffib :: Integer -> Integer
ffib' :: Integer -> Integer -> Integer -> Integer -> Integer
ffib' n k fk fk1 | k == n  = fk
                 | k < n   = ffib' n (k + 1) (fk + fk1) fk
ffib 1 = 1
ffib n = ffib' n 2 1 1


count_e :: Double -> Double
count_e' :: Double -> Double -> Double -> Double -> Double
count_e eps = count_e' eps 1 1 0
count_e' eps n lastmemb cursum | lastmemb < eps  = cursum
                               | otherwise       = count_e' eps (n+1) (lastmemb/n) (cursum + lastmemb)


main = do
{-    print (twice 5)

    print(factorial1 5)
    print(factorial2 5)
    print(factorial3 5)

    print(-9, mod 9 6, mod 6 9)
    print(gcd2 2 3, gcd2 3 6, gcd2 6 3, gcd2 6 0)
    print(gcd1 2 3, gcd1 3 6, gcd1 6 3, gcd1 6 0)
    print(gcd1 9 (-9)) 

    print(isprime 3, isprime 33)
    print(isprime 2)
    print(isprime 0)
    print(isprime (-4))

    print("Naive algo running...  ")    
    print(fib 30)
    print("Fast algo running...   ")
    print(ffib 300)

    print(count_e 2, count_e 1, count_e 0.1, count_e 0.01, count_e 0.001)
-}
    print("Ali" == ['A', 'l', 'i'], [1..10], [2,5..14], [2,5..15])
    print([1,2,3] ++ [8,8], 4:6:8 : [1,2,3], 2:[])

