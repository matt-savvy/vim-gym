{-
Use moving to a line number as part of a motion.
Starting at padRight, delete both padRight and padLeft in
one motion, targeting the last line number of padLeft.

Make sure to preserve an empty line between double and triple.
-}

double :: (Num a) => a -> a
double = (* 2)

padRight :: String -> Int -> String
padRight str len
    | length str >= len = str
    | otherwise = take len $ str <> repeat ' '

padLeft :: String -> Int -> String
padLeft str len =
    let
        amountToPad = max 0 (len - length str)
     in
        replicate amountToPad ' ' <> str

triple :: (Num a) => a -> a
triple = (* 3)
