data Student = Student {sec_name :: String, grades :: [(String, Integer)]} deriving (Show)

instance Eq Student where
  (Student s1 g1) == (Student s2 g2) = get_av_grade g1 == get_av_grade g2 && (not (is_dvoich g1 && is_dvoich g2) || (is_dvoich g1 && is_dvoich g2))

instance Ord Student where
  (Student s1 g1) < (Student s2 g2) = get_av_grade g1 < get_av_grade g2 || (is_dvoich g1 && not (is_dvoich g2))
  (Student s1 g1) <= (Student s2 g2) = (Student s1 g1) < (Student s2 g2) || (Student s1 g1) == (Student s2 g2)

get_av_grade :: [(String, Integer)] -> Integer
get_av_grade [] = 0
get_av_grade ((_, y) : tail) = y + get_av_grade tail

is_dvoich :: [(String, Integer)] -> Bool
is_dvoich [] = False
is_dvoich ((_, y) : tail) =
  if y == 2
    then True
    else is_dvoich tail

main :: IO ()
main = do
  let st1 = Student {sec_name = "Ivanov", grades = [("Math", 5), ("Prog", 5), ("History", 2), ("History", 4)]}
  let st2 = Student {sec_name = "Sidorov", grades = [("Math", 5), ("Prog", 5), ("History", 2)]}
  let st3 = Student {sec_name = "Sergeev", grades = [("Math", 5)]}
  let st4 = Student {sec_name = "Semenov", grades = [("Math", 5), ("Prog", 5), ("History", 4)]}

  print (st1 > st2)
  print (st1 == st2)
  print (st3 == st4)
  print (st4 < st3)
  print (st4 >= st3)
  print (st1 <= st3)