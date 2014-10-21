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
