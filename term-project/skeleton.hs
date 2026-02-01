import Codec.Picture
import System.Directory (createDirectoryIfMissing)

gridSize :: Int
gridSize = 50

cellSize :: Int
cellSize = 10

imageSize :: Int
imageSize = gridSize * cellSize

colorFor :: Int -> PixelRGB8
colorFor 0 = PixelRGB8 255 255 255  -- white
colorFor 1 = PixelRGB8 0 0 255      -- blue
colorFor _ = PixelRGB8 0 0 0        -- error!

-- Change these functions for different patterns

-- decide if cell lives or dies
shouldLive :: Int -> Int -> Bool
shouldLive currentState neighborCount
  | currentState == 1 && (neighborCount == 2 || neighborCount == 3) = True -- case 1: alive and has 2 or 3 friends, stay alive
  | currentState == 0 && neighborCount == 3 = True -- case 2: dead but has 3 friends, wake up
  | otherwise = False -- all other, die or stay dead

-- count how many alive neighbors for (x, y)
countNeighbors :: [[Int]] -> Int -> Int -> Int
countNeighbors grid x y = 
  let height = length grid -- how many rows
      width = length (head grid) -- how many columns
      neighbors = [ (x + dx, y + dy) -- go around 8 directions
                  | dx <- [-1,0,1], dy <- [-1,0,1], 
                    not (dx == 0 && dy == 0) -- skip self
                  ]
      validNeighbors = filter (\(nx, ny) -> nx >= 0 && ny >= 0 && nx < width && ny < height) neighbors
        -- keep only inside-grid ones
  in sum [ grid !! ny !! nx | (nx, ny) <- validNeighbors ] -- add up live cells



-- make next gen grid using rules
nextGeneration :: [[Int]] -> [[Int]]
nextGeneration grid = 
  let height = length grid
      width = length (head grid)
  in [ [ if shouldLive (grid !! y !! x) (countNeighbors grid x y)  -- check rule for each cell
           then 1 -- make alive
           else 0 -- make dead
       | x <- [0..width-1] ] -- loop x
     | y <- [0..height-1] ] -- loop y


-- put small pattern in center of big grid
centerPattern :: [[Int]] -> [[Int]]
centerPattern pattern = 
  let patternHeight = length pattern -- how tall pattern
      patternWidth = length (head pattern)-- how wide
      offsetY = (gridSize - patternHeight) `div` 2 -- top space
      offsetX = (gridSize - patternWidth) `div` 2  -- left space
  in [ [ if y >= offsetY && y < offsetY + patternHeight && 
             x >= offsetX && x < offsetX + patternWidth
         then (pattern !! (y - offsetY)) !! (x - offsetX) -- copy pattern value
         else 0 -- empty outside
       | x <- [0..gridSize-1] ] -- for x
     | y <- [0..gridSize-1] ] -- for y


-- run pattern for n steps
evolvePattern :: [[Int]] -> Int -> [[Int]]
evolvePattern initial 0 = centerPattern initial -- if 0 steps, just center
evolvePattern initial n = evolvePattern (nextGeneration initial) (n - 1)
  -- else go one step and call again (recursion)


-- Pattern 1: Pinwheel oscillator
pattern1 :: Int -> [[Int]]
pattern1 generations = evolvePattern pinwheelInitial generations
  where
    pinwheelInitial = 
      [ [0,0,0,0,0,0,1,1,0,0,0,0]  -- 6b2o
      , [0,0,0,0,0,0,1,1,0,0,0,0]  -- 6b2o
      , [0,0,0,0,0,0,0,0,0,0,0,0]  -- empty line
      , [0,0,0,0,1,1,1,1,0,0,0,0]  -- 4b4o
      , [1,1,0,1,0,0,1,0,1,0,0,0]  -- 2obo2bobo
      , [1,1,0,1,0,1,0,0,1,0,0,0]  -- 2obobo2bo
      , [0,0,0,1,0,0,0,1,1,0,1,1]  -- 3bo3b2ob2o
      , [0,0,0,1,0,0,0,0,1,0,1,1]  -- 3bo4bob2o
      , [0,0,0,0,1,1,1,1,0,0,0,0]  -- 4b4o
      , [0,0,0,0,0,0,0,0,0,0,0,0]  -- empty line
      , [0,0,0,0,1,1,0,0,0,0,0,0]  -- 4b2o
      , [0,0,0,0,1,1,0,0,0,0,0,0]  -- 4b2o
      ]



-- Pattern 2: Queen Bee Shuttle oscillator
pattern2 :: Int -> [[Int]]
pattern2 generations = evolvePattern queenBeeShuttleInitial generations
  where
    queenBeeShuttleInitial = 
      [ [0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0]  -- 9b2o
      , [0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0]  -- 9bobo
      , [0,0,0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,1]  -- 4b2o6bo7b2o
      , [1,1,0,1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,1,1]  -- 2obo2bo2bo2bo7b2o
      , [1,1,0,0,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0]  -- 2o2b2o6bo
      , [0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0]  -- 9bobo
      , [0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0]  -- 9b2o
      ]



-- Pattern 3: Worker Bee
pattern3 :: Int -> [[Int]]
pattern3 generations = evolvePattern pattern3Initial generations
  where
    pattern3Initial = 
      [
      [1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1],
      [0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0],  -- bo12bo
      [0,1,0,1,0,0,0,0,0,0,0,0,1,0,1,0],  -- bobo8bobo
      [0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0],  -- 2b2o8b2o2
      [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  -- 5b6o2
      [0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0],  -- 2b2o8b2o
      [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  -- bobo8bobo
      [0,0,1,1,0,0,0,0,0,0,0,0,1,1,0,0],  -- bo12bo
      [0,1,0,1,0,0,0,0,0,0,0,0,1,0,1,0],  -- 2o12b2o
      [0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0],
      [1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1]
      ]



-- Pattern 4: toad hassler
pattern4 :: Int -> [[Int]]
pattern4 generations = evolvePattern pattern4Initial generations
  where
    pattern4Initial = 
      [
      [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1],  -- 2o16b2o
      [0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0],  -- bo7b2o7bo
      [0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,1,0],  -- bobo3bo4bo3bobo
      [0,0,1,1,0,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0],  -- 2b2o2bo6bo2b2o
      [0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0],  -- 5bo8bo
      [0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0],  -- 5bo8bo
      [0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0],  -- 5bo8bo
      [0,0,1,1,0,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0],  -- 2b2o2bo6bo2b2o
      [0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,1,0],  -- bobo3bo4bo3bobo
      [0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0],  -- bo7b2o7bo
      [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1],  -- 2o16b2o
      [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0],  -- 11bo
      [0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0],  -- 9bo2bo
      [0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0],  -- 9bo2bo
      [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0],  -- 10bo
      [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1],  -- 2o16b2o
      [0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0],  -- bo7b2o7bo
      [0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,1,0],  -- bobo3bo4bo3bobo
      [0,0,1,1,0,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0],  -- 2b2o2bo6bo2b2o
      [0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0],  -- 5bo8bo
      [0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0],  -- 5bo8bo
      [0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0],  -- 5bo8bo
      [0,0,1,1,0,0,1,0,0,0,0,0,0,1,0,0,1,1,0,0],  -- 2b2o2bo6bo2b2o
      [0,1,0,1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,1,0],  -- bobo3bo4bo3bobo
      [0,1,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,1,0],  -- bo7b2o7bo
      [1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1]  -- 2o16b2o
      ]



-- Pattern 5: Gourmet
pattern5 :: Int -> [[Int]]
pattern5 generations = evolvePattern pattern5Initial generations
  where
    pattern5Initial = 
      [
      [0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0],  -- 10b2o
      [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0],  -- 10bo
      [0,0,0,0,1,1,0,1,1,0,1,0,0,0,0,1,1,0,0,0],  -- 4b2ob2obo4b2o
      [0,0,1,0,0,1,0,1,0,1,0,0,0,0,0,1,0,0,0,0],  -- 2bo2bobobo5bo
      [0,0,1,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0],  -- 2b2o4bo8bo
      [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0],  -- 16b2o2
      [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  -- 16b2o
      [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0],  -- o9b3o2bobo
      [1,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,0,1,0,0],  -- 3o7bobo3bo
      [1,1,1,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0],  -- 3bo6bobo4b3o
      [0,0,0,1,0,0,0,0,0,0,1,0,1,0,0,0,0,1,1,1],  -- 2bobo14bo
      [0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],  -- 2b2o2
      [0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  -- 2b2o
      [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  -- 2bo8bo4b2o
      [0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],  -- 4bo5bobobo2bo
      [0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0],  -- 3b2o4bob2ob2o
      [0,0,0,0,1,0,0,0,0,0,1,0,1,0,1,0,0,1,0,0],  -- 9bo
      [0,0,0,1,1,0,0,0,0,1,0,1,1,0,1,1,0,0,0,0],  -- 8b2o
      [0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0],
      [0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0]
      ]


matrixToImage :: [[Int]] -> Image PixelRGB8
matrixToImage mat = generateImage pixelAt imageSize imageSize
  where
    pixelAt x y =
      let row = y `div` cellSize
          col = x `div` cellSize
          val = (mat !! row) !! col
      in colorFor val

main :: IO ()
main = do
  let patternFuncs = zip [1..]
        [ pattern1
        , pattern2
        , pattern3
        , pattern4
        , pattern5
        ]

  mapM_
    (\(i, patFn) -> do
        let dirName = "frames" ++ show i
        createDirectoryIfMissing True dirName
        putStrLn $ "Folder Created: " ++ dirName

        mapM_
          (\j -> do
              let matrix = patFn j
              let image = matrixToImage matrix
              let fileName = dirName ++ "/frame" ++ show j ++ ".png"
              savePngImage fileName (ImageRGB8 image)
              putStrLn $ "Saved: " ++ fileName
          )
          [0..70]
    )
    patternFuncs