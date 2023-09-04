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
