import System.Environment

data Axis = X Double | Y Double | Z Double | A Double | B Double | C Double
    deriving (Eq, Read, Show)

data Offset = I Double | J Double | K Double
    deriving (Eq, Read, Show)

linear :: Axis -> Bool
linear (X _) = True
linear (Y _) = True
linear (Z _) = True
linear _ = False

main :: IO ()
main = getArgs >>= print . haqify . head
 
haqify = (++) "Haq! "
