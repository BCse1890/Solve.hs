module M1Lecture4 where

buildNumberBetter :: [Word] -> Word
buildNumberBetter digits = buildNumberBetterTail (length digits) 0 digits

buildNumberBetterTail :: Int -> Word -> [Word] -> Word
buildNumberBetterTail _ acc [] = acc
buildNumberBetterTail n acc (x:xs) =  
  buildNumberBetterTail (n-1) (acc + (x * 10 ^ (n-1))) xs
 
        
maxesAndMins :: [Int] -> (Int, Int)
maxesAndMins [] = (0,0)
maxesAndMins (x:xs) = maxMinTail x 0 x 0 (x:xs)

maxMinTail :: Int -> Int -> Int -> Int -> [Int] -> (Int, Int)
maxMinTail _ newMinCount _ newMaxCount [] = (newMaxCount, newMinCount)
maxMinTail newMin newMinCount newMax newMaxCount (x:xs) 
   | x > newMax = maxMinTail newMin newMinCount x (newMaxCount + 1) xs
   | x < newMin = maxMinTail x (newMinCount+1) newMax newMaxCount xs
   | otherwise  = maxMinTail newMin newMinCount newMax newMaxCount xs
 
   
elevationRedux :: String -> Maybe Int
elevationRedux  = flight (0,0)
  where flight (height, index) input
          | height < 0 = Just index
          | null input = Nothing
          | head input == 'u' = flight (height+100, index+1) (tail input)
          | head input == 'd' = flight (height-100, index+1) (tail input)
          | otherwise = flight (height, index+1) (tail input)


fireworks :: (Int, Int) -> Int -> Int -> [Int] -> [Int] -> Int
fireworks (h1, h2) n1 n2 f1s f2s =  

{-   let
    fireWorksTail_n1 :: (Int, Int) -> Int -> Int -> [Int] -> [Int] ->Int -> Int 
    fireWorksTail_n1 (_, _) _ _ [] _ acc = acc
    fireWorksTail_n1 (h1, h2) n1 n2 (x:xs) f2s acc
      | (h1 <= n1+x && h2 >= n1+x) = fireWorksTail_n1 (h1, h2) n1 n2 xs f2s (acc+1)
      | otherwise = fireWorksTail_n1 (h1, h2) n1 n2 xs f2s (acc) -}

  let
    fireWorksTail_n1 :: [Int] ->Int -> Int 
    fireWorksTail_n1 [] acc_n1 = acc_n1
    fireWorksTail_n1 (x:xs) acc_n1
      | (h1 <= n1+x && h2 >= n1+x) = fireWorksTail_n1 xs (acc_n1+1)
      | otherwise = fireWorksTail_n1  xs (acc_n1)

    fireWorksTail_n2 :: [Int] ->Int -> Int 
    fireWorksTail_n2 [] acc_n2 = acc_n2
    fireWorksTail_n2 (d:ds) acc_n2
      | (h1 <= n2+d && h2 >= n2+d) = fireWorksTail_n2 ds (acc_n2+1)
      | otherwise = fireWorksTail_n2 ds (acc_n2)
      
  in fireWorksTail_n1 f1s 0  + fireWorksTail_n2 f2s 0


math2d :: [[Int]] -> Int
math2d = rowScan 0
  where
    rowScan :: Int -> [[Int]] -> Int
    rowScan acc [] = acc
    rowScan acc (thisrow: rest) = rowScan (insideScan acc False thisrow) rest

    -- calculate Score across the row
    insideScan :: Int -> Bool -> [Int] -> Int
    insideScan acc _ [] = acc
    insideScan acc True (y:ys) = insideScan (acc * y) False ys
    insideScan acc False (y: ys) = insideScan (acc + y) True ys


tripletSums :: [Int] -> Int
tripletSums = tripTail 0 
  
tripTail :: Int -> [Int] -> Int
tripTail acc [] = acc
tripTail acc [x] = acc + x
tripTail acc [y, z] = acc + y + z
tripTail acc (m:n:q: qs) = tripTail (acc + tripl) qs
  where tripl = (m + n) `mod` q

-- tripletSums [6, 7, 4, 2, 35, 17, 3, 4]
