import           Data.Matrix
import qualified Data.Vector as V
import           Data.List as L
import           System.Environment
import           Codec.Picture
import           Codec.Picture.Types
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Debug.Trace as Tr

-- By Ink Brownell
-- A basic renderer to display wireframes of
-- .obj files, demonstrating the use of linear algebra
-- in computer graphics

width  = 1920 
height = 1080

horizontalFOV = 90.0 :: Float 


type PolarPoint    = (Float, Float, Float)
type Point         = V.Vector Float
type Vertex        = V.Vector Float
type VertexIndices = V.Vector Int
data RenderType    = Orthographic | Perspective deriving (Show, Eq, Read)


verticalFOV = 2 * atan (tan(horizontalFOV / 2 * pi / 360) * (fromIntegral width / fromIntegral height)) * 360 / (2 * pi):: Float

toRectangularCoords :: PolarPoint -> Point
toRectangularCoords (r, az, inc) = V.fromList 
    [ r * dsin (90 - inc) * dsin az
    , r * dcos (90 - inc)
    , r * dsin(90 - inc) * dcos az
    ] 

pointInsideScreen :: (Int, Int) -> Bool
pointInsideScreen (a, b) =
    a >= 0
    && b >= 0
    && a < width 
    && b < height

onScreen :: [(Int, Int)] -> Bool
onScreen l = case l of
    x : xs -> pointInsideScreen x || onScreen xs
    []     -> True

dsin a = sin (a * 2 * pi / 360)
dcos a = cos (a * 2 * pi / 360)
dtan a = dsin a / dcos a        


toScreenCoord :: Vertex -> (Int, Int)
toScreenCoord v = (((width `div` 2) +)  $ floor $ (( * 0.5) . fromIntegral $ width) * (v V.! 0)
                  , ((height `div` 2) +) $ floor $ (( * (-0.5)) . fromIntegral $ height) * (v V.! 1))

removeComments :: String -> String
removeComments s = case s of 
    ""       -> ""
    '#' : xs -> ""
    x   : xs -> x : removeComments xs


parseStrings :: [[String]] -> ([Vertex], [VertexIndices])
parseStrings s = revFirst $ parseStrings' s [] []
    where revFirst (a, b) = (reverse a, b)

parseStrings' :: [[String]] -> [Vertex] -> [VertexIndices] -> ([Vertex], [VertexIndices])
parseStrings' []   v i = (v, i)
parseStrings' (x : xs) v i = case com of
    "v" -> parseStrings' xs (new:v) i
        where new = V.fromList (map read $ tail x :: [Float])
    "f" -> parseStrings' xs v     ([new] ++ i)
        where new = V.fromList (map (pos . read . extract) $ tail x :: [Int])
              pos = (\y -> if y < 0 then
                               length v + y
                               else
                                   y - 1) :: Int -> Int
    a   -> parseStrings' xs v i
  where com = x !! 0


extract :: String -> String
extract s = case s of 
        ""       -> ""
        '/' : xs -> ""
        x   : xs -> x : extract xs


pointsOfLine' :: (Int, Int) -> (Int, Int) -> Int -> Int -> Int-> Int -> Int -> [(Int, Int)]
pointsOfLine' p0@(x0, y0) p1@(x1, y1) x y dx dy derr 
        | x > x1 = []
        | derr > 0  = (x, y) : pointsOfLine' p0 p1 (x + 1) (y + 1) dx dy (derr - 2 * dx + 2 * dy)
        | otherwise = (x, y) : pointsOfLine' p0 p1 (x + 1) y       dx dy (derr + 2 * dy)





pointsOfLine :: (Int, Int) ->(Int, Int) -> [(Int, Int)]
pointsOfLine p0@(x0, y0) p1@(x1, y1) = func 
        where 
            dx = x1 - x0
            dy = y1 - y0
            derr = 2*dy - dx
            octant 
                | dx >= 0 && dy >= 0 && dx  >=  dy = 0
                | dx >= 0 && dy >= 0 && dx  <   dy = 1
                | dx <  0 && dy >= 0 && -dx <   dy = 2
                | dx <  0 && dy >= 0 && -dx >=  dy = 3
                | dx <  0 && dy <  0 && -dx >= -dy = 4
                | dx <  0 && dy <  0 && -dx <  -dy = 5
                | dx >= 0 && dy <  0 && dx  <  -dy = 6
                | dx >= 0 && dy <  0 && dx  >= -dy = 7
            func = case octant of 
                0 -> map invFunc $ pointsOfLine' p0         p1         x0    y0     dx    dy    derr
                1 -> map invFunc $ pointsOfLine' (y0,   x0) (y1,   x1) y0    x0     dy    dx    (2 * dx - dy)
                2 -> map invFunc $ pointsOfLine' (y0,  -x0) (y1,  -x1) y0    (-x0)  dy    (-dx) (-2 * dx - dy)
                3 -> map invFunc $ pointsOfLine' (-x0,  y0) (-x1 , y1) (-x0) y0     (-dx) dy    (2 * dy + dx)
                4 -> map invFunc $ pointsOfLine' (-x0, -y0) (-x1, -y1) (-x0) (-y0)  (-dx) (-dy) (-2 * dy + dx)
                5 -> map invFunc $ pointsOfLine' (-y0, -x0) (-y1, -x1) (-y0) (-x0)  (-dy) (-dx) (-2 * dx + dy)
                6 -> map invFunc $ pointsOfLine' (-y0,  x0) (-y1,  x1) (-y0)  x0    (-dy) dx    (2 * dx + dy)
                7 -> map invFunc $ pointsOfLine' (x0,  -y0) (x1,  -y1) x0     (-y0) dx    (-dy) (-2 * dy - dx)
            invFunc = case octant of 
                0 -> \ (x, y) -> (x,   y)
                1 -> \ (x, y) -> (y,   x)
                2 -> \ (x, y) -> (-y,  x)
                3 -> \ (x, y) -> (-x,  y)
                4 -> \ (x, y) -> (-x, -y)
                5 -> \ (x, y) -> (-y, -x)
                6 -> \ (x, y) -> (y,  -x)
                7 -> \ (x, y) -> (x,  -y)
                                          

setImage :: [(Int, Int)] -> Image Pixel8
setImage l = runST $ do 
        image <- createMutableImage width height (0 :: Pixel8)
        let writeImage a
                | null a = unsafeFreezeImage image
                | x >= 0 
                  && x < width
                  && y >= 0
                  && y < height = 
                      do 
                          writePixel image x y 255
                          writeImage $ tail a
                | otherwise = writeImage $ tail a
                where 
                    (x, y) = head a
        writeImage l


main = do 
        args <- getArgs
        if length args < 6 
            then error "Not enough args, required args are, in order:\n* Input file (file must be a .obj file)\n* Output file (output will be a .png)\n* Scale factor\n* Camera radial distance\n* Camera azimuth angle (in degrees)\n* Camera inclination angle (in degrees)"
            else putStrLn "Sufficient arguments"


        let inputFile    = args !! 0                           :: String
        let outputFile   = args !! 1                           :: String
        let scaleFactor  = read $ args !! 2                    :: Float
        let cameraRadius = read $ args !! 3                    :: Float
        let az           = 2 * pi / 360 * read ( args !! 4 )   :: Float
        let inc          = 2 * pi / 360 * read ( args !! 5 )   :: Float

        let cameraCoords = toRectangularCoords (cameraRadius, az * 360 / (2 * pi), inc * 360 / (2 * pi)) :: Point
        
        let n = 1.0                         :: Float
        let f = 1000.0                        :: Float
        let r = n * dtan(horizontalFOV / 2) :: Float
        let t = n * dtan(verticalFOV / 2)   :: Float
        contents <- readFile inputFile 

          
           
        let tokens   = filter (/= []) . map ( words . removeComments ) $ lines contents :: [[String]]
        
        let (objectSpaceVertices, faces) = parseStrings tokens



         


          
        let objToCameraSpaceMatrix = (translationMatrix cameraRadius) * (inclinationMatrix inc) * (azimuthMatrix az) * (scale3Matrix scaleFactor) :: Matrix Float

        let cameraSpaceVertices    = map (getMatrixAsVector . ( objToCameraSpaceMatrix * ) . colVector . flip V.snoc 1.0) objectSpaceVertices :: [Vertex]

        let transformMatrix        = projectionTransformMatrix r t n f
          
        let clipSpaceVertices      = map ( getMatrixAsVector . ( transformMatrix * ) . colVector ) cameraSpaceVertices :: [Vertex]
          
        let ndcVertices            = map scale clipSpaceVertices :: [Vertex]
                where scale v = V.fromList [(v V.! 0) / (v V.! 3), (v V.! 1) / (v V.! 3), (v V.! 2) / (v V.! 3)] 
          
        let screenSpaceVertices    = V.fromList $ map toScreenCoord ndcVertices :: V.Vector (Int, Int)

        let facesInScSp            = filter onScreen $ map (V.toList . V.map (screenSpaceVertices V.! )) faces :: [[(Int, Int)]]

        let endpoints              = concatMap lines facesInScSp :: [((Int, Int), (Int, Int))]
                where lines (x:y:xs) = (x, y) : lines (y:xs)
                      lines x        = []

        let pointsOnLine           = concatMap (unpack pointsOfLine) endpoints :: [(Int, Int)]
                where unpack f (a, b) = f a b

        let finalImage             = setImage pointsOnLine

        writePng outputFile finalImage


-- ##############################
-- ##                          ##
-- ##  Here Be Linear Algebra  ##
-- ##                          ##
-- ##############################


projectionTransformMatrix :: Float -> Float -> Float -> Float -> Matrix Float
projectionTransformMatrix r t n f =
        fromLists 
            [ [ n/r, 0,    0,            0            ]
            , [ 0,   n/t,  0,            0            ]
            , [ 0,   0,    -(f+n)/(f-n), -2*f*n/(f-n) ]
            , [ 0,   0,    -1,           0            ]
            ]


azimuthMatrix :: Float -> Matrix Float
azimuthMatrix az =
        fromLists 
            [ [ cos (-az),  0, sin (-az), 0 ]
            , [ 0,          1, 0,         0 ]
            , [ -sin (-az), 0, cos (-az), 0 ]
            , [ 0,          0, 0,         1 ]
            ]


inclinationMatrix :: Float -> Matrix Float
inclinationMatrix inc =
        fromLists 
            [ [ 1, 0,       0,        0 ]
            , [ 0, cos inc, -sin inc, 0 ]
            , [ 0, sin inc, cos inc,  0 ]
            , [ 0, 0,       0,        1 ]
            ]


translationMatrix :: Float -> Matrix Float
translationMatrix r = 
        fromLists 
            [ [ 1, 0, 0, 0  ]
            , [ 0, 1, 0, 0  ]
            , [ 0, 0, 1, -r ]
            , [ 0, 0, 0, 1  ]
            ]

scale3Matrix :: Float -> Matrix Float
scale3Matrix x =
        fromLists
            [ [ x, 0, 0, 0 ]
            , [ 0, x, 0, 0 ]
            , [ 0, 0, x, 0 ]
            , [ 0, 0, 0, 1 ]
            ]
