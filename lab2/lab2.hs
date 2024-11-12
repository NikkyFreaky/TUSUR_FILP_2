-- Задание а
funcA :: [[Integer]] -> Integer
funcA spis = head [x | (x,y) <- zip [1..] spis2, y == maximum spis2 || y == minimum spis2 ] where spis2 = concat spis

-- Задание б
funcB :: [Integer] -> Integer
funcB spis = maximum(map (\(x,y) -> x + y) (zip spis (tail spis)))

main :: IO ()
main = do
    print $ funcA [[12,4,12], [-3,5], [-4, 15]]
    print $ funcB [2, 3, 5, 6, 7, 5, 8, 9, 3, 0, 1]