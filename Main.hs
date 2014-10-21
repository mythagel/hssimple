import System.Environment
{-|
 - Holy crap, compare with
 - https://github.com/mythagel/cxxcam/blob/master/include/Axis.h
 - https://github.com/mythagel/cxxcam/blob/master/src/Axis.cpp
 - Which achieves exactly the same thing...
 -}
--data Axis = X Double | Y Double | Z Double | A Double | B Double | C Double
--    deriving (Eq, Read, Show)

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



main :: IO ()
main = getArgs >>= print . haqify . head
 
haqify = (++) "Haq! "
