-- import System.Environment
{-|
 - Holy crap, compare with
 - https://github.com/mythagel/cxxcam/blob/master/include/Axis.h
 - https://github.com/mythagel/cxxcam/blob/master/src/Axis.cpp
 - Which achieves exactly the same thing...
 -}
--data Axis = X Double | Y Double | Z Double | A Double | B Double | C Double
--    deriving (Eq, Read, Show)

-- TODO implement custom show instance 
-- which outputs words as required for gcode
-- (generally truncated to 6 digits)
data Word = A Double 
          | B Double
          | C Double
          | D Integer
          | F Double
          | G Double
          | H Integer
          | I Double
          | J Double
          | K Double
          | L Double
          | M Double
          | P Double
          | Q Double
          | R Double
          | S Double
          | T Integer
          | X Double
          | Y Double
          | Z Double
    deriving (Eq, Read, Show)

data Units = Metric | Imperial
    deriving (Eq, Read, Show)
data Plane = XY | ZX | YZ | UV | WU | VW
    deriving (Eq, Read, Show)
data CoordinateSystem = Active | P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9
    deriving (Eq, Read, Show)
data Motion = Absolute | Incremental
    deriving (Eq, Read, Show)
data ArcMotion = ArcAbsolute | ArcIncremental
    deriving (Eq, Read, Show)
-- ...

g00 = G 0
g01 = G 1
g02 = G 2
g03 = G 3
g04 = G 4
g17 = G 17
g18 = G 18
g19 = G 19
g17_1 = G 17.1
g18_1 = G 18.1
g19_1 = G 19.1
g20 = G 20
g21 = G 21
g40 = G 40
g49 = G 49
g54 = G 54
g55 = G 55
g56 = G 56
g57 = G 57
g58 = G 58
g59 = G 59
g59_1 = G 59.1
g59_2 = G 59.2
g59_3 = G 59.3
-- ...

axis :: Word -> Bool
axis (X _) = True
axis (Y _) = True
axis (Z _) = True
axis (A _) = True
axis (B _) = True
axis (C _) = True
axis _     = False

linearAxis :: Word -> Bool
linearAxis (X _) = True
linearAxis (Y _) = True
linearAxis (Z _) = True
linearAxis _ = False

offset :: Word -> Bool
offset (I _) = True
offset (J _) = True
offset (K _) = True
offset _     = False

planeWord :: Plane -> Word
planeWord XY = g17
planeWord ZX = g18
planeWord YZ = g19
planeWord UV = g17_1
planeWord WU = g18_1
planeWord VW = g19_1

unitsWord :: Units -> Word
unitsWord Imperial = g20
unitsWord Metric = g21

coordinateSystemWord :: CoordinateSystem -> Word
coordinateSystemWord Active = error "Cannot change to Active coordinate system"
coordinateSystemWord P1 = g54
coordinateSystemWord P2 = g55
coordinateSystemWord P3 = g56
coordinateSystemWord P4 = g57
coordinateSystemWord P5 = g58
coordinateSystemWord P6 = g59
coordinateSystemWord P7 = g59_1
coordinateSystemWord P8 = g59_2
coordinateSystemWord P9 = g59_3

motionWord :: Motion -> Word
motionWord Absolute = G 90
motionWord Incremental = G 91

arcMotionWord :: ArcMotion -> Word
arcMotionWord ArcAbsolute = G 90.1
arcMotionWord ArcIncremental = G 91.1

preamble :: Plane -> Units -> CoordinateSystem -> Motion -> ArcMotion -> [Word]
preamble plane units cs motion arc = [g00, planeWord plane, unitsWord units, g40, g49, coordinateSystemWord cs, G 80, motionWord motion, arcMotionWord arc]

main :: IO ()
main = do
    let x = X 20
        isLinear = linearAxis x
    putStrLn $ show isLinear
    putStrLn $ show $ preamble XY Metric P1 Absolute ArcIncremental





