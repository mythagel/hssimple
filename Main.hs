import System.Environment

data Axis = X | Y | Z | A | B | C
    deriving (Eq, Read, Show, Enum)

data Offset = I | J | K
    deriving (Eq, Read, Show, Enum)

linear :: Axis -> Bool
linear X = True
linear Y = True
linear Z = True
linear _ = False

main :: IO ()
main = getArgs >>= print . haqify . head
 
haqify = (++) "Haq! "
