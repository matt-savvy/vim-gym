-- Use a macro and global command to convert Floats to use type class Num, e.g
-- double :: Num a => a -> a

double :: Float -> Float
double = (*) 2

addOne :: Float -> Float
addOne = (+) 1

doubleAndPlusOne :: Float -> Float
doubleAndPlusOne = addOne . double

plusOneAndDouble :: Float -> Float
plusOneAndDouble = double . addOne
