-- Задание а
funcA :: [Integer] -> ([Integer], [Integer])
funcA [] = ([], [])
funcA (x : xs)
  | x > 0 = (x : pos, neg)
  | x < 0 = (pos, x : neg)
  where
    (pos, neg) = funcA xs

-- Задание б
funcB :: [Integer] -> Integer
funcB [] = undefined
funcB [_] = undefined
funcB [x, _] = x
funcB (_ : xs) = funcB xs

main :: IO ()
main = do
  print $ funcA [1, 2, -3, 4, -5, 10, -15, -1]
  print $ funcB [1, 2, 3, -4, 5, 0, 1]