module M1Lecture6 where

import Prelude hiding (Either(..))
import Utils

volumeAndSurfaceArea :: [(Int, Int, Int)] -> (Int, Int)
volumeAndSurfaceArea = foldl folding (0, 0) 
  where 
    folding (accV, accS) (x,y,z) = (accV + (x*y*z), accS + (2*x*y + 2*x*z + 2*y*z))


countMostAdjacent :: [(Int, Int)] -> Int
countMostAdjacent [] = 0
countMostAdjacent ((val, count):xs) = output
  where
    (_, _ , output) = foldl fn (val, count, count) xs
    fn (prevV, prevC, accC)  (v, c) = 
      if (v == prevV+1) 
        then (v, c,  max (prevC+c) accC)
        else (v, c, max c accC)

robotHits :: Int -> Int -> [Int] -> (Int, Int, Int)
robotHits x0 y0 spikes = (xwins, ywins, draws)
  where
    (_, _, xwins, ywins, draws) = foldl f (x0, y0, 0, 0, 0) spikes
    f (xposn, yposn, xacc, yacc, drawacc) p = 
      let
        xdist = abs (p - xposn)
        ydist = abs (p - yposn)
      in if (xposn == p) then (p, yposn, xacc+1, yacc, drawacc)
        else if (yposn == p) then (xposn, p, xacc, yacc+1, drawacc)
        else if (xdist == ydist) then (p-2, p+2, xacc, yacc, drawacc+1)
        else if (xdist < ydist) 
          then (p, loserPosn yposn p xdist, xacc+1, yacc, drawacc)
          else (loserPosn xposn yposn ydist, p, xacc, yacc+1, drawacc)

    loserPosn robotposn spikeposn winDistance =
      if (robotposn < spikeposn) then (robotposn + winDistance)
      else if (robotposn > spikeposn) then (robotposn - winDistance)
      else spikeposn 

 
math2d :: [[Int]] -> Int
math2d  = foldl f_vert 0   
  where
    f_vert accV [] = accV
    f_vert accV p =  fst $ foldl f_horiz (accV, True) p
    f_horiz (accH, evenCol) y = if evenCol then (accH+y, False) else (accH*y, True)


-- revisit this problem : not always clear what is being asked here
seamCarving :: [[Int]] -> Int
seamCarving [] = 0
seamCarving (r1:restRows) = minimum $ foldl f_vert r1 restRows
  where
    f_vert :: [Int] -> [Int] -> [Int]
    f_vert previous_row row = reverse $ snd $ foldl f_horiz (maxBound : previous_row, []) row
    f_horiz :: ([Int], [Int]) -> Int -> ([Int], [Int])
    f_horiz (prev_row, acc) xcurrent = case (prev_row) of
      [x,y] -> ([y] , xcurrent + min x y : acc)
      (x:y:z: prev_row') -> (  y:z:prev_row', xcurrent + (min x (min y z)) : acc)
      _ -> error "input array of insufficient length"


      
manhattanTravel2d :: [Coord2f] -> Double -> Coord2f
manhattanTravel2d [] _ = (0.0, 0.0)

manhattanTravel2d coordList d = finalCoord
  where
    journey = reverse $ foldl calcDistDirn [] (zip coordList (tail coordList))
    (_, finalCoord) = foldl travel (distToTravel, head coordList) journey

    calcDistDirn :: [(Double, Direction4)] -> (Coord2f, Coord2f) -> [(Double, Direction4)]
    calcDistDirn acc ((x1, y1), (x2, y2)) = 
      let ymovt = (abs (y2 - y1), if y2 > y1 then Up else Down) 
          xmovt = (abs (x2 - x1), if x2 > x1 then Right else Left)
      in xmovt : ymovt : acc
    
    distToTravel =  d * (sum $ fmap fst journey)

    travel :: (Double, Coord2f) -> (Double, Direction4) -> (Double, Coord2f)                   
    travel (remDist, (x, y)) (dist, dirn) = if remDist >= dist 
      then (remDist - dist, moveD4f dist (x, y) dirn) 
      else (0, moveD4f remDist (x,y) dirn)
        
    


