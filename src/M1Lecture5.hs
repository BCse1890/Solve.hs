module M1Lecture5 where

enumerate :: [a] -> [(Int, a)]
enumerate' xs = [(x,y)  | (x, y) <- zip [0..] xs]

enumerate = zip [0..]
        

calculateVelocities :: [(Double, Double)] -> [Double]
calculateVelocities [] = []
calculateVelocities xs = calculateVelocitiesTail [] xs

calculateVelocitiesTail :: [Double] -> [(Double, Double)] -> [Double]
calculateVelocitiesTail acc [] = reverse acc
calculateVelocitiesTail acc [_] = reverse acc
calculateVelocitiesTail acc ((x1, t1):(x2, t2): input') = 
    calculateVelocitiesTail (v: acc) ((x2, t2): input') 
  where v = (x2-x1) / (t2-t1)

palindrome :: String -> Bool
palindrome input = reverse f == f
  where f = filter (/= ' ') input


-- remember that when input is cited in outer function call it is in scope for 
-- helper function and does not need to be called as a parameter in helper ie

allFactors :: [Int] -> [Int]
allFactors [] = []
allFactors inList = allFactorsTail [] [1.. (maximum inList)]
  where
    allFactorsTail ::  [Int] -> [Int] -> [Int]
    allFactorsTail acc [] = reverse acc
    allFactorsTail acc (p:ps) = case (any  (\ x -> x `mod` p == 0) inList ) of
      True -> allFactorsTail (p:acc) ps 
      False -> allFactorsTail acc ps

   

-- makeDigits :: Word -> [Word]


makeDigits :: Word -> [Word]
makeDigits a = reverse (makeDigitsH a)
makeDigitsH 0 = []
makeDigitsH a 
   | a < 10 = [r]
   | otherwise = r: makeDigitsH t
    where (t, r) = a `quotRem` 10

canStartShed :: Int -> Int -> [Int] -> Bool
canStartShed startTime requiredPackages arrivalTimes = cssTail 0 arrivalTimes
  where
    cssTail n [] = ( n >= requiredPackages )
    cssTail n (x:xs)
      | x <= startTime = cssTail (n+1) xs
      | otherwise = cssTail n xs   
      
