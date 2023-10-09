-- Substitute Float with Int below

double :: Float -> Float
double = (*) 2

addOne :: Float -> Float
addOne = (+) 1

doubleAndPlusOne :: Float -> Float
doubleAndPlusOne = addOne . double

plusOneAndDouble :: Float -> Float
plusOneAndDouble = double . addOne
