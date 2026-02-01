{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import GHC.Exts.Heap (IndexTable(it_back_edge), GenClosure (n_args))
import Data.List
import Data.Ord

type Point = (Float,Float) -- 3D point locations
type Line = (Point, Point, Float) -- Two points for a line and the width
type Triangle = (Point, Point, Point) --To save STL files

sortClockwise :: [Point] -> [Point]
sortClockwise ps = sortBy (flip $ comparing (angle c)) ps
  where
    c = (avg (map fst ps), avg (map snd ps))
    avg xs = sum xs / fromIntegral (length xs)
    angle (cx, cy) (x, y) = atan2 (y - cy) (x - cx)


line_list :: [Line]
line_list = [((-5,-0.25),(5,0.25),1)]

create_triangles_from_rectangle :: [Point] -> [Triangle]
create_triangles_from_rectangle corners = [tri1, tri2, tri3, tri4]
    where
        ([x1,x2,x3,x4], [y1,y2,y3,y4]) = unzip(sortClockwise corners)
        middle_point = ((x1+x2+x3+x4) /4, (y1+y2+y3+y4)/4)
        tri1 = ((x1,y1), middle_point, (x2,y2))
        tri2 = ((x2,y2), middle_point, (x3,y3))
        tri3 = ((x3,y3), middle_point, (x4,y4))
        tri4 = ((x4,y4), middle_point, (x1,y1))


get_rectangle_corners :: Line -> [(Float,Float)]
get_rectangle_corners ((x1,y1), (x2,y2), width) = [(x1 + hwx, y1 + hwy), (x1 - hwx, y1 - hwy), (x2 - hwx, y2 - hwy), (x2 + hwx, y2 + hwy)]
    where
        length = sqrt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1))
        nx = -(y2 - y1) / length
        ny = (x2 - x1) / length
        hwx = nx * width / 2
        hwy = ny * width / 2 

linelist_to_rects :: [Line] -> [Triangle]
linelist_to_rects line_list = concat [create_triangles_from_rectangle (get_rectangle_corners line) | line <- line_list]

createTriangleDef :: Triangle -> String
createTriangleDef ((x1,y1),(x2,y2),(x3,y3)) =
    "  facet\n" ++
    "    outer loop\n" ++
    "      vertex " ++ (show x1) ++ " " ++ (show y1) ++ " 0 \n" ++
    "      vertex " ++ (show x2) ++ " " ++ (show y2) ++ " 0 \n" ++
    "      vertex " ++ (show x3) ++ " " ++ (show y3) ++ " 0 \n" ++
    "    endloop\n" ++
    "  endfacet\n"                            

createObjectModelString :: [Triangle] -> String 
createObjectModelString n = "solid Object01\n" ++ concat [createTriangleDef y | y<-n] ++ "endsolid Object01"



writeObjModel :: [Triangle] -> String -> IO ()
writeObjModel x filename = do writeFile filename (createObjectModelString x)



hilbertcurve :: Integer -> [Line]
hilbertcurve order =

  let
    spanLen     = 10.0                       :: Float
    size        = (2 :: Int) ^ order
    steps       = size - 1
    stepLen     = spanLen / fromIntegral steps
    startPos    = (-spanLen/2, spanLen/2)    :: Point
    initState   = (startPos, 0)              :: (Point, Float)
    (_, segments) = hilbertTurtle order (pi/2) stepLen initState
  in
    segments

  where
    lineWidth :: Float
    lineWidth = 0.05

    hilbertTurtle
      :: Integer            
      -> Float               
      -> Float              
      -> (Point,Float)        
      -> ((Point,Float), [Line]) -- state of point and list of lines completed

    hilbertTurtle 0 _ _ st = (st, [])
    hilbertTurtle lvl ang step st0 =
      let
        turnRight (p,d) = (p, d - ang)
        turnLeft  (p,d) = (p, d + ang)

        forward ((x,y), d) =  -- point and step is the paramters used, the ouput is a line
          let nx = x + step * cos d
              ny = y + step * sin d
          in (((nx,ny), d), [((x,y),(nx,ny), lineWidth)]) -- the output line calculated

        -- recursion here 
        st1        = turnRight st0
        (st2,s1)   = hilbertTurtle (lvl-1) (-ang) step st1
        (st3,f1)   = forward st2
        st4        = turnLeft st3
        (st5,s2)   = hilbertTurtle (lvl-1)  ang   step st4
        (st6,f2)   = forward st5
        (st7,s3)   = hilbertTurtle (lvl-1)  ang   step st6
        st8        = turnLeft st7
        (st9,f3)   = forward st8
        (st10,s4)  = hilbertTurtle (lvl-1) (-ang) step st9
        st11       = turnRight st10

        combined = s1 ++ f1 ++ s2 ++ f2 ++ s3 ++ f3 ++ s4
      in
        (st11, combined) -- returned values



main :: IO ()
main = do writeObjModel (linelist_to_rects (hilbertcurve 7)) "hilbert.stl" 

