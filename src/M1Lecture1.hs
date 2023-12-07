module M1Lecture1 where

import Utils

sum3 :: [Int] -> Int
sum3 [] = 0
sum3 [x] = x
sum3 (a:b:[]) = a + b
sum3 (a:b:c:rest) = a + b + c

firstTravel :: [Coord2] -> Int
firstTravel [] = 0
firstTravel [(x, y)] = 0
firstTravel ((x1, y1): (x2, y2): _) = abs(x1 - x2) + abs(y1 - y2)
