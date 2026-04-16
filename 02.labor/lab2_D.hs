-- # 2. labor

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat 0 = 1
szjSzorzat x
  | x< 0 = error " neg szam"
  | otherwise = mod x 10 * szjSzorzat (div x 10)

szjSzorzat2 x
  |x<0 = error "neg szam"
  |x == 0 =1
  |otherwise = mod x 10 * szjSzorzat (div x 10)
-- - egy szám számjegyeinek összegét (2 módszerrel),
-- szám adott számjegyeinek összege
szamszjosszeg :: Int -> Int -> Int
szamszjosszeg 0 _ = 0
szamszjosszeg n szj
  | mod n 10 == szj = szj + szamszjosszeg (div n 10) szj
  | otherwise = szamszjosszeg (div n 10) szj


-- páros számjegyek száma
parosSzjDb :: Int -> Int
parosSzjDb 0 = 0
parosSzjDb n
  | mod (mod n 10) 2 == 0 = 1 + parosSzjDb (div n 10)
  | otherwise = parosSzjDb (div n 10)


-- legnagyobb számjegy
maxSzj :: Int -> Int
maxSzj n
  | n < 10 = n
  | otherwise =
      let m = maxSzj (div n 10)
          d = mod n 10
      in if d > m then d else m


-- számjegyek száma b számrendszerben
fugv :: Int -> Int -> Int -> Int
fugv 0 _ _ = 0
fugv n b d
  | mod n b == d = 1 + fugv (div n b) b d
  | otherwise = fugv (div n b) b d


-- Fibonacci
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fib1000 :: Integer
fib1000 = fib 1000


main :: IO()
main = do
    let fel1 = szjSzorzat 1234
    print fel1

    print (szamszjosszeg 577723707 7)

    print (parosSzjDb 12345678)

    print (maxSzj 9834512)

    print (fugv 7673573 10 7)
    print (fugv 1024 2 1)
    print (fugv 1023 2 1)
    print (fugv 345281 16 4)

    print fib1000

    print (map (\x -> szamszjosszeg x 7) [123,456,789])
    print (map szjSzorzat [123,456,789])
    print (map parosSzjDb [1234,2222,1357])