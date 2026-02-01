import System.Environment (getArgs)
import System.IO
import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX (getPOSIXTime)


-- some parameters arent passed so im defining them here instead
grid_x, grid_y :: Int
grid_x = 200
grid_y = 200

threshold :: Double
threshold = 0.4

radius_neighbour :: Int
radius_neighbour = 5

maxSteps :: Int
maxSteps = 1000

-- i use a matrix by having list of integer lists
type Grid = [[Int]]

-- the input comes as a one dimentional list so im converting it into grid
toGrid :: [Int] -> Grid
toGrid xs = [ take grid_y (drop (i * grid_y) xs) | i <- [0 .. grid_x - 1] ]

-- I flatten it to return the output the same way i recieved it
flattenGrid :: Grid -> [Int]
flattenGrid = concat

-- this is because I couldnt inclue random library, 
-- I still dont know what the problem was but even if I install it still said it is not installed
randomR' :: Int -> Int -> (Int, Int)
randomR' seed range =
  let nextSeed = (1103515245 * seed + 12345) `mod` 2147483648
      rnd = nextSeed `mod` range
  in (rnd, nextSeed)

-- This is to shuffle when iterating to find a happy spot
shuffle :: [a] -> IO [a]
shuffle xs = do
  t <- getPOSIXTime
  let seed0 = floor (t * 1000)
  return $ shuffle' seed0 xs
  where
    shuffle' _ [] = []
    shuffle' seed ys =
      let (i, seed') = randomR' seed (length ys)
          x = ys !! i
          ys' = take i ys ++ drop (i + 1) ys
      in x : shuffle' seed' ys'

-- this is to find the neighbors in a given radius
neighbors :: Grid -> Int -> Int -> [Int]
neighbors grid i j =
  [ grid !! r !! c
    | di <- [-radius_neighbour .. radius_neighbour]
    , dj <- [-radius_neighbour .. radius_neighbour]
    , let r = i + di
    , let c = j + dj
    , (di, dj) /= (0,0)
    , r >= 0, r < grid_x
    , c >= 0, c < grid_y
  ]

-- this is to determine if it will be happy or not there based on the neighbours found with above function
isHappyAt :: Grid -> Int -> Int -> Int -> Bool
isHappyAt grid i j t =
  let neigh = filter (/= 0) (neighbors grid i j)
      sameCount = length (filter (== t) neigh)
      total = length neigh
  in total == 0 || fromIntegral sameCount / fromIntegral total >= threshold

-- this function allows to return a cells bool of happyness based on its neighbors vs treshold
isHappyCell :: Grid -> Int -> Int -> Bool
isHappyCell grid i j =
  let t = grid !! i !! j
  in t == 0 || isHappyAt grid i j t

-- this returns every empty position
emptyPositions :: Grid -> [(Int, Int)]
emptyPositions grid =
  [ (i,j)
    | i <- [0..grid_x-1]
    , j <- [0..grid_y-1]
    , grid !! i !! j == 0
  ]

-- this function is to replace the current cells type with the other one chosen (0 1 2)
replaceCell :: Grid -> (Int, Int) -> Int -> Grid
replaceCell grid (i,j) val =
  take i grid
  ++ [ take j (grid !! i) ++ [val] ++ drop (j + 1) (grid !! i) ]
  ++ drop (i + 1) grid

-- this function handles the grid manipulation in the simulation recursive function
timeStepIO :: Grid -> IO Grid
timeStepIO grid = do
  coords  <- shuffle [ (i,j) | i <- [0..grid_x-1], j <- [0..grid_y-1] ] -- shuffle all cell positions so that we will look for each cells happyness randomly
  empties <- shuffle (emptyPositions grid) -- shuffle empty positions
  return $ go grid empties coords
  where
    go g _ [] = g -- base case: no more cells to check
    go g empties ((i,j):rest)
      | g !! i !! j == 0 = go g empties rest
      | isHappyCell g i j = go g empties rest
      | otherwise = -- do changes if a cell is not empty and unhappy
          let t = g !! i !! j
              findSpot [] = (Nothing, empties) -- if no valid spot found dont change
              findSpot (e:es) -- search for a happy spot
                | isHappyAt g ei ej t = (Just e, delete e empties) -- if the cell makes it happy return the spot
                | otherwise = findSpot es -- if not look in the rest of the list
                where (ei,ej) = e
              (mSpot, newEmpties) = findSpot empties
          in case mSpot of
              Just spot -> -- if returned a spot relocate type to new happy spot found
                let g' = replaceCell (replaceCell g spot t) (i,j) 0
                in go g' newEmpties rest -- if a happy spot was found and moved, continue with updated grid and empties
              Nothing -> go g empties rest -- if no happy spot found, leave the agent in place and continue


-- this is a recursive function, iterates as long as the max steps not reached and all cells are not happy.
simulateIO :: Grid -> Int -> IO Grid
simulateIO grid stepCount
  | stepCount >= maxSteps = return grid -- if max step reached
  | otherwise = do
      grid' <- timeStepIO grid
      if all (\(i,j) -> isHappyCell grid' i j) [ (i,j) | i <- [0..grid_x-1], j <- [0..grid_y-1] ] 
        then return grid' -- if all of them is happy
        else simulateIO grid' (stepCount + 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputFile, outputFile] -> do
      contents <- readFile inputFile
      let ints  = map read (lines contents) :: [Int]
          grid0 = toGrid ints -- convert the input to matrix
      finalGrid <- simulateIO grid0 0 -- call recursive simulating function
      let outLines = map show (flattenGrid finalGrid) -- convert the output to list
      writeFile outputFile (unlines outLines)

    