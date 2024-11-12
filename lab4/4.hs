import System.IO
import Data.List

data Rating = Rating {name :: String, grades :: [Integer]} deriving (Show,Eq,Read)

--Чтение из файла
readFromFile :: FilePath -> IO [Rating]
readFromFile f = do
    h <- openFile f ReadMode
    rating <- loop h []
    hClose h
    return rating
    where loop h acc = do
                isEof <- hIsEOF h
                if not isEof
                    then do
                            b <- hGetLine h
                            let rating  = read b :: Rating 
                            loop h (rating:acc)
                    else return acc

--Сортировка группы по рейтингу
sumGrades :: Rating -> Integer
sumGrades (Rating _ grades) = sum grades

compareRating :: Rating -> Rating -> Ordering
compareRating r1 r2 = compare (sumGrades r2) (sumGrades r1)

sortRating :: [Rating] -> [Rating]
sortRating = sortBy compareRating

--Запись в файл
addGrade :: String -> Integer -> [Rating] -> [Rating]
addGrade _ _ [] = []
addGrade n g (r:rs) | name r == n = r {grades = grades r ++ [g]} : rs
                    | otherwise = r : addGrade n g rs

writeToFile :: FilePath -> [Rating] -> IO ()
writeToFile f rs = do
    writeFile f (unlines (map show rs))


main :: IO ()
main = do
    let path = "rating.txt"
    rating <- readFromFile path
    putStrLn "Rating:"
    print rating

    let updatedRating = addGrade "Zlobin" 5 rating
    writeToFile path updatedRating
    new_rating <- readFromFile path
    putStrLn "\nUpdated rating:"
    print new_rating

    let sortedRating = sortRating updatedRating
    putStrLn "\nSorted rating:"
    print sortedRating