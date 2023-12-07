module M1Lecture3 where

makeDigits :: Word -> [Word]
makeDigits 0 = []
makeDigits a 
   | a < 10 = [r]
   | otherwise = r: makeDigits t
    where (t, r) = a `quotRem` 10


buildNumber :: [Word] -> Word
buildNumber [] = 0
buildNumber (x:xs) = ( x * 10 ^ (l-1)) + buildNumber xs
    where l = length (x:xs)

elevation :: String -> Int
elevation "" = 0
elevation (x:xs) 
  | x == 'u' = 100 + (elevation xs)
  | x == 'd' = -100 + (elevation xs)
  | otherwise = error "Input not recognised. Input either 'u' or 'd'."

viralCount :: Int -> Int -> Int
viralCount 0 _ = 0
viralCount _ 0 = 0
viralCount friends hours = shares + (viralCount friends' (hours-1))
  where
    shares = friends `div` 3
    friends' = shares * 4
        

addQuads :: [Int] -> [Int] -> Int
addQuads [] [] = 0
addQuads [x] ys = addQuads ([x, 1]) ys
addQuads [] ys = addQuads [1,1] ys
addQuads xs [y] = addQuads xs ([y, 1])
addQuads xs [] = addQuads xs ([1,1])
addQuads (x1:x2:xs) (y1:y2:ys)
   | otherwise = (x1*x2*y1*y2) + addQuads xs ys


countChars :: String -> Int
countChars [] = 0
countChars ('\\': 'x': _ : _ : xs) = 1 + countChars xs
countChars ('\\': 'o': _ : _ : _ : xs) = 1 + countChars xs
countChars ('\\': _ : xs) = 1 + countChars xs
countChars (_ : xs) = 1 + countChars xs



