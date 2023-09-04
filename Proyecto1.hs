--10. La funci ́on primIgualesA toma un valor y una lista, y calcula el tramo inicial m ́as largo de la lista cuyos elementos son iguales a ese valor.
--10.a) Program ́a primIgualesA por recursi ́on.
primIgualesA :: a -> [a]  -> [a]
primIgualesA _ [] = []
primIgualesA n (x:xs) | n == x = x:primIguales n xs
                      | n /= x = primIgualesA n []

--10.b) Program ́a nuevamente la funci ́on utilizando takeWhile.
