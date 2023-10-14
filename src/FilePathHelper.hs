module FilePathHelper (split) where

split :: FilePath -> (FilePath, FilePath)
split fullPath = split' ("", fullPath)

split' :: (String, String) -> (String, String)
split' (path, file) =
    if '/' `elem` file
        then
            let
                (x : xs) = file
                path' = path <> [x]
             in
                split' (path', xs)
        else (path, file)
