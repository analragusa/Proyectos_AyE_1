--1) defini las siguientes funciones:
--a) esCero :: Int -> Bool, que verifica si un entero es igual a 0.
esCero :: Int -> Bool
esCero x  | x == 0 = True
          | otherwise = False
--b) esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0.         
esPositivo :: Int -> Bool
esPositivo x | x>0 = True
             | otherwise = False
--c) esVocal :: Char -> Bool, que verifica si un caracter es una vocal en min ́uscula.             
esVocal :: Char -> Bool
esVocal x = x `elem`  ['a', 'e', 'i', 'o', 'u']


valorAbsoluto :: Int -> Int
valorAbsoluto x = abs x

paraTodo :: [Bool] -> Bool
paraTodo (x : xs) | (x == True) = paraTodo xs
                  | otherwise = False

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

factorial :: Int -> Int
factorial x | x == 0 = 1
            | x == 1 = 1
            | otherwise = x * factorial (x - 1)
            
promedio :: [Int] -> Int
promedio [] = 0
promedio xs = (sumatoria xs) `div` (length xs)

pertenece :: Int -> [Int] -> Bool
pertenece m [] = False
pertenece m (x:xs) | (m == x) = True
                   | (m /= x) = pertenece m xs
                  
--paraTodo' :: [a] -> (a -> Bool) -> Bool
--paraTodo' [] n = True
--paraTodo' (x:xs) n | paraTodo' xs n
--                   | otherwise = False
                   
                                      
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] a = False
existe' (x:xs) a | (a x) = existe' xs a == True


sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] e = 0
sumatoria' (x:xs) e = (e x) + sumatoria' xs e

producto' :: [a] -> (a -> Int) -> Int
producto' [] n = 1
producto' (x:xs) n = (n x) * producto' xs n

paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] b = True
paraTodo' (x:xs) b = paraTodo' xs b


par :: Int -> Bool
par n = mod n 2 == 0

todosPar :: [Int] -> Bool
todosPar [] = True
todosPar xs = paraTodo' xs (par)


multiplo :: Int -> Int -> Bool
multiplo b y = b `mod` y == 0

hayMult :: Int -> [Int] -> Bool
hayMult b xs = existe' xs (multiplo b)


cuad :: Int -> Int
cuad n = n*n

sumaCuadrados :: Int -> Int
sumaCuadrados m = sumatoria' [1..(m-1)] (cuad)


diviDe :: Int -> [Int] -> Bool
diviDe b (x:xs) = mod x b == 0 || diviDe b xs

existeDivisor :: Int -> [Int] -> Bool
existeDivisor n [] = False
existeDivisor n xs = hayMult n xs

esPrimo :: Int -> Bool
esPrimo n = (existeDivisor n [2..(n-1)] || n < 1) == False



predicado :: Int -> Int 
predicado n = n+0 


factorial' :: Int -> Int
factorial' n = producto' [1..n] (predicado)



--6.g) Programar la funci ́on multiplicaPrimos :: [Int] -> Int que calcula el producto de todos los n ́umeros primos de una lista.

filPri :: Int -> Int
filPri n | esPrimo n == True = n
         | otherwise = 1


multiPrimos :: [Int] -> Int
multiPrimos xs = producto' xs (filPri)



fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
      

esFib :: Int -> Bool
esFib n = n `elem` [fib i | i <- [0..(n+1)]]


todosFib :: [Int] -> Bool
todosFib xs = paraTodo' xs (esFib)


--8. Programa una funci ́on que dada una lista de n ́umeros xs, devuelve la lista que resulta de duplicar cada valor de xs.

--8.a)
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (x * 2) : duplica xs

--8.b)
duplica' :: [Int] -> [Int]
duplica' xs = map (*2) xs

-- 9. Programa una funci ́on que dada una lista de n ́umeros xs, calcula una lista que tiene como elementos aquellos n ́umeros de xs que son primos.
--9.a)






--10. La funci ́on primIgualesA toma un valor y una lista, y calcula el tramo inicial m ́as largo de la lista cuyos elementos son iguales a ese valor.
--10.a) Program ́a primIgualesA por recursi ́on.
primIgualesA :: a -> [a]  -> [a]
primIgualesA _ [] = []
primIgualesA n (x:xs) | n == x = x:primIguales n xs
                      | n /= x = primIgualesA n []

--10.b) Program ́a nuevamente la funci ́on utilizando takeWhile.
primIgualesA' :: a -> [a] -> [a]
primIgualesA' n xs = takeWhile (n == x)

--11. La funci ́on primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos elementos son todos iguales entre s ́ı.
--11.a)
primIguales :: [a] -> [a]
primIguales [] = []
primIguales (x:xs) = | x == (xs!!0) = x:primIguales xs
                     | x /= (xs!!0) = primIguales []

--11.b) Us ́a cualquier versi ́on de primIgualesA para programar primIguales. Est ́a permitido dividir en casos, pero no usar recursi ́on.
primIguales' :: [a] -> [a]
primIguales' xs = primIgualesA (xs!!0) xs
