
--1) defini las siguientes funciones:
--a) esCero :: Int -> Bool, que verifica si un entero es igual a 0.
esCero :: Int -> Bool
esCero x  | x == 0 = True
          | otherwise = False

--ghci> esCero 7
--False
--ghci> esCero 0
--True

--b) esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0.         
esPositivo :: Int -> Bool
esPositivo x | x>0 = True
             | otherwise = False

--ghci> esPositivo 8
--True
--ghci> esPositivo (-9)
--False

--c) esVocal :: Char -> Bool, que verifica si un caracter es una vocal en min ́uscula.             
esVocal :: Char -> Bool
esVocal x = x `elem`  ['a', 'e', 'i', 'o', 'u']

--ghci> esVocal 'a'
--True
--ghci> esVocal 'g'
--False

--d) valorAbsoluto :: Int -> Int, que devuelve el valor absoluto de un entero ingresado.
valorAbsoluto :: Int -> Int
valorAbsoluto x = abs x

--ghci> valorAbsoluto 8
--8
--ghci> valorAbsoluto (-10)
--10

--2) Program ́a las siguientes funciones usando recursi ́on o composici ́on:
--a) paratodo :: [Bool] -> Bool, que verifica que todos los elementos de una lista
-- sean T
paraTodo :: [Bool] -> Bool
paraTodo (x : xs) | (x == True) = paraTodo xs
                  | otherwise = False

--ghci> paraTodo [True, True, False]
--False
--ghci> [False, False]
--False

-- b) sumatoria :: [Int] -> Int, que calcula la suma de todos los elementos de una
-- lista de enteros.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--ghci> sumatoria [4, 5, 6, 7]
--22
--ghci> sumatoria []
--0

-- c) productoria :: [Int] -> Int, que calcula el producto de todos los elementos de
-- la lista de enteros.
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

--ghci> productoria [1, 2, 1, 5]
--10
--ghci> productoria []
--1

-- d) factorial :: Int -> Int, que toma un n ́umero n y calcula n!.
factorial :: Int -> Int
factorial x | x == 0 = 1
            | x == 1 = 1
            | otherwise = x * factorial (x - 1)

--ghci> factorial 5
--120
--ghci> factorial 2
--2

-- e) Utiliz ́a la funci ́on sumatoria para definir, promedio :: [Int] -> Int, que toma
-- una lista de n ́umeros no vacia y calcula el valor promedio (truncado, usando divisi ́on
-- entera).
promedio :: [Int] -> Int
promedio [] = 0
promedio xs = (sumatoria xs) `div` (length xs)

--ghci> promedio [8, 5, 7, 1, 6]
--5
--ghci> promedio [4, 2]
--3

-- 3) Program ́a la funci ́on pertenece :: Int -> [Int] -> Bool, que verifica si un n ́umero se
-- encuentra en una lista.
pertenece :: Int -> [Int] -> Bool
pertenece m [] = False
pertenece m (x:xs) | (m == x) = True
                   | (m /= x) = pertenece m xs

--ghci> pertenece 9 [8, 6, 2, 4, 7, 9]
--True
--ghci> pertenece 5 [4, 2, 3, 8]

-- 4) Program ́a las siguientes funciones que implementan los cuantificadores generales. Not ́a que
-- el segundo par ́ametro de cada funci ́on, es otra funci ́on!                  
-- a) paratodo’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un
-- predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el
-- predicado t.
paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] n = True
paraTodo' (x:xs) n | paraTodo' x n
                   | otherwise = False

--ghci> paraTodo' [6, 6, 6, 5] even
--False
--ghci> paraTodo' [6, 6, 6] esPositivo
-- True

--b) existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado 
-- t :: a -> Bool, determina si alg ́un elemento de xs satisface el predicado t.                                      
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] a = False
existe' (x:xs) a | (a x) = existe' xs a == True

--ghci> existe' [8, 5, 2] (==2)
--True
--ghci> existe' [8, 8, 2, 5, 4, 1, 36, 9] (==7)
--False

--c) sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una
--funci ́on t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la
--suma de los valores que resultan de la aplicaci ́on de t a los elementos de xs.
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] e = 0
sumatoria' (x:xs) e = (e x) + sumatoria' xs e

--ghci> sumatoria' [4, 1, 1, 4] (+2)
--18
--ghci> sumatoria' [2, 2, 2, 3] (*1)
--9

--d) producto’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a]
--y una funci ́on t :: a -> Int, calcula el producto de los valores que resultan de la
--aplicaci ́on de t a los elementos de xs.
producto' :: [a] -> (a -> Int) -> Int
producto' [] n = 1
producto' (x:xs) n = (n x) * producto' xs n

--ghci> producto' [1, 1, 2, 2] (+1)
--36
--ghci> producto' [1, 1, 1, 1, 1, 5] (*1)
--5

--5. Defin ́ı nuevamente la funci ́on paratodo, pero esta vez usando la funci ́on paratodo’ (sin
--recursi ́on ni an ́alisis por casos!).
paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] b = True
paraTodo' (x:xs) b = paraTodo' xs b

--ghci> paraTodo' [True, False] (==True)
--False
--ghci> paraTodo' [8, 8] (==8)
--True

--6. Utilizando las funciones del ejercicio 4, program ́a las siguientes funciones por composici ́on,
--sin usar recursi ́on ni an ́alisis por casos.
--a) todosPares :: [Int] -> Bool verifica que todos los n ́umeros de una lista sean
--pares.
par :: Int -> Bool
par n = mod n 2 == 0

todosPar :: [Int] -> Bool
todosPar [] = True
todosPar xs = paraTodo' xs (par)

--ghci> todosPar [4, 2, 6, 8, 24]
--True
--ghci> todosPar [5, 2, 4]
--False

--b) hayMultiplo :: Int -> [Int] -> Bool verifica si existe alg ́un n ́umero dentro del
--segundo par ́ametro que sea m ́ultiplo del primer par ́ametro.

multiplo :: Int -> Int -> Bool
multiplo b y = b `mod` y == 0

hayMult :: Int -> [Int] -> Bool
hayMult b xs = existe' xs (multiplo b)

--ghci> hayMult 2 [36, 5, 2]
--True
--ghci> hayMult 5 [4, 2, 6, 9, 5]
--True

-- c) sumaCuadrados :: Int -> Int, dado un n ́umero no negativo n, calcula la suma de
--los primeros n cuadrados
cuad :: Int -> Int
cuad n = n*n

sumaCuadrados :: Int -> Int
sumaCuadrados m = sumatoria' [1..(m-1)] (cuad)

--ghci> sumaCuadrados 5
--29
--ghci> sumaCuadrados 6
--54

--Programar la fuci ́on existeDivisor::Int-> [Int] -> Bool, que dado en entero n
--y una lista ls , devuelve True si y solo si, existe alg ́un elemento en ls que divida a na.
diviDe :: Int -> [Int] -> Bool
diviDe b (x:xs) = mod x b == 0 || diviDe b xs

existeDivisor :: Int -> [Int] -> Bool
existeDivisor n [] = False
existeDivisor n xs = hayMult n xs

--ghci> existeDivisor 2 [5, 1, 9, 6]
--True
--ghci> existeDivisor 3 [5, 4, 8, 10]
--False

--Utilizando la funci ́on del apartado anterior, defin ́ı la funci ́on esPrimo:: Int -> Bool,
--que dado un entero n, devuelve True si y solo si n es primo.
esPrimo :: Int -> Bool
esPrimo n = (existeDivisor n [2..(n-1)] || n < 1) == False

--ghci> esPrimo 5
--True
--ghci> esPrimo 6
--False

-- f) ¿Se te ocurre como redefinir factorial (ej. 2d) para evitar usar recursi ́on?

predicado :: Int -> Int 
predicado n = n+0 

factorial' :: Int -> Int
factorial' n = producto' [1..n] (predicado)

--ghci> factorial' 5
--120
--ghci> factorial' 4
--24

--6.g) Programar la funci ́on multiplicaPrimos :: [Int] -> Int que calcula el producto de todos los n ́umeros primos de una lista.

filPri :: Int -> Int
filPri n | esPrimo n == True = n
         | otherwise = 1

multiPrimos :: [Int] -> Int
multiPrimos xs = producto' xs (filPri)

--ghci> multiPrimos [5, 1, 2, 6, 4, 8]
--10
--ghci> multiPrimos [2, 2, 3, 6]
--12

-- h) Programar la funci ́on esFib :: Int -> Bool, que dado un entero n, devuelve True
--si y s ́olo si n est ́a en la sucesi ́on de Fibonacci.
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)     

esFib :: Int -> Bool
esFib n = n `elem` [fib i | i <- [0..(n+1)]]

--ghci> esFib 8
--True
--ghci> esFib 5
--True

--i) Utilizando la funci ́on del apartado anterior, defin ́ı la funci ́on todosFib :: [Int] -> Bool
--que dada una lista xs de enteros, devuelva si todos los elementos de la lista pertenecen
--(o no) a la sucesi ́on de Fibonacci.
todosFib :: [Int] -> Bool
todosFib xs = paraTodo' xs (esFib)

--ghci> todosFib [144, 6, 2]
--False
--ghci> todosFib [55, 8, 13]
--True

--7) Indag ́a en Hoogle sobre las funciones map y filter. Tambi ́en podes consultar su tipo en
--ghci con el comando :t.
--¿Qu ́e hacen estas funciones?

--MAP es una f de orden superior que toma una función (que a su vez ésta toma un a y un b,) 
--y una lista xs y aplica esa función a cada elemento de xs, produciendo una nueva lista. 
--FILTER es una f que toma un predicado y una lista, 
--devolviendo una lista con los elementos que satisfacen el predicado. Es decir, si p x se evalua
--en True, x es incluido a la lista.

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [b]
filter p [] = []
filter p (x:xs) | p x = x : filter p xs
                | otherwise = filter p xs 

--¿A qu ́e equivale la expresi ́on map succ [1, -4, 6, 2, -8], donde succ n = n+1?

--Equivale a la lista [2,-3,7,3,-7], donde cada elemento es el siguiente de la lista dada.


--¿Y la expresi ́on filter esPositivo [1, -4, 6, 2, -8]?

-- Lista de los positivos [1,6,2] pertenecientes a la lista dada.


--8. Programa una funci ́on que dada una lista de n ́umeros xs, devuelve la lista que resulta de duplicar cada valor de xs.

--8.a) Definila usando recursi ́on.
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (x * 2) : duplica xs

--ghci> duplica [1, 2, 1, 2]
--[2, 4, 2, 4]
--ghci> duplica [5, 2]
--[10, 4]

--8.b) Definila utilizando la funci ́on map.
duplica' :: [Int] -> [Int]
duplica' [] = []
duplica' xs = map (*2) xs

--ghci> duplica' [5, 8, 4]
--[10, 16, 8]
--ghci> dupica' []
--[]

-- 9. Programa una funci ́on que dada una lista de n ́umeros xs, calcula una 
--lista que tiene como elementos aquellos n ́umeros de xs que son primos.
--9.a)

listaPrim :: [Int] -> [Int]
listaPrim [] = []
listaPrim (x:xs) | esPrimo x = x: listaPrim xs
                 | otherwise = listaPrim xs             

--ghci> listaPrim [8, 5, 4]
--False
--ghci> listaPrim [2, 3, 3, 5]
--True

--10. La funci ́on primIgualesA toma un valor y una lista, y calcula el tramo inicial m ́as largo de la lista cuyos elementos son iguales a ese valor.
--10.a) Program ́a primIgualesA por recursi ́on.
primIgualesA :: a -> [a]  -> [a]
primIgualesA _ [] = []
primIgualesA n (x:xs) | n == x = x:primIguales n xs
                      | n /= x = primIgualesA n []

--ghci> primIgualesA 5 [4, 2, 5, 5, 5, 8]
--[]
--ghci> primIgualesA 'k' "kkkkkl"
--kkkkk

--10.b) Program ́a nuevamente la funci ́on utilizando takeWhile.
primIgualesA' :: a -> [a] -> [a]
primIgualesA' n xs = takeWhile (n == x)

--ghci> primIgualesA' 5 [5, 5, 2]
--[5, 5]
--ghci> primIgualesA' 2 [2, 5, 4, 2, 2, 2]
--[2]

--11. La funci ́on primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos elementos son todos iguales entre s ́ı.
--11.a)
primIguales :: [a] -> [a]
primIguales [] = []
primIguales (x:xs) = | x == (xs!!0) = x:primIguales xs
                     | x /= (xs!!0) = primIguales []

--ghci> primIguales [5, 8, 4]
--[5]
--gchi> primIguales [2, 2, 2, 3]
--[2, 2, 2]

--11.b) Us ́a cualquier versi ́on de primIgualesA para programar primIguales. Est ́a permitido dividir en casos, pero no usar recursi ́on.
primIguales' :: [a] -> [a]
primIguales' xs = primIgualesA (xs!!0) xs

--ghci> primIguales' [6, 6, 6, 3]
--[6, 6, 6]
--ghci> primIguales' [0, 0, 2]
--[0, 0]

--
