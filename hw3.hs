-- Author: Dunh Adam Lee
-- Date: 07/18/2024
-- ECS 140A HW3


---------------------------------------------------------------------------------------------------------------------------
-- Problem 1 
myremovedduplicates :: Eq a => [a] -> [a] 
myremovedduplicates student_data
    | null student_data = []
    | head student_data `elem` tail student_data = myremovedduplicates (tail student_data)
    | otherwise = (head student_data) : myremovedduplicates (tail student_data)

myremovedduplicates_pm :: Eq a => [a] -> [a]
myremovedduplicates_pm [] = []
myremovedduplicates_pm (x:xs)
    | x `elem` xs = myremovedduplicates xs
    | otherwise = x : myremovedduplicates xs
---------------------------------------------------------------------------------------------------------------------------
-- Problem 2
myintersection :: Eq a => [a] -> [a] -> [a]
myintersection student_data1 student_data2
    | null student_data1 = []
    | null student_data2 = []
    | head student_data1 `elem` student_data2 = (head student_data1) : myintersection (tail student_data1) student_data2
    | otherwise = myintersection (tail student_data1) student_data2

myintersection_pm :: Eq a => [a] -> [a] -> [a]
myintersection_pm _ [] = []
myintersection_pm [] _ = []
myintersection_pm (x:xs) (y:ys)
    | x `elem` y:ys = myintersection xs (y:ys)
    | otherwise = myintersection xs (y:ys)
---------------------------------------------------------------------------------------------------------------------------
-- Problem 3
mynthtail :: Eq a => Int -> [a] -> [a]
mynthtail n student_data
    | null student_data = []
    | n == 0 = student_data
    | otherwise = mynthtail (n - 1) (tail student_data)

mynthtail_pm :: Eq a => Int -> [a] -> [a]
mynthtail_pm 0 (x:xs) = x:xs
mynthtail_pm _ [] = []
mynthtail_pm n (x:xs) = mynthtail (n - 1) xs
---------------------------------------------------------------------------------------------------------------------------
-- Problem 4
mylast :: Eq a => [a] -> [a]
mylast student_data
    | null student_data = [] -- base case: return an empty list if input is empty
    | null (tail student_data) = [head student_data] -- base case: return the first element if tail is empty
    | otherwise = mylast (tail student_data) -- recursive call: run mylast again with the tail

mylast_pm :: Eq a => [a] -> [a]
mylast_pm [] = []
mylast_pm (x:[]) = [x]
mylast_pm (x:xs) = mylast xs
---------------------------------------------------------------------------------------------------------------------------
-- Problem 5
myreverse :: Eq a => [a] -> [a]
myreverse student_data = myreversehelper student_data []

myreversehelper :: Eq a => [a] -> [a] -> [a]
myreversehelper student_data1 student_data2
    | null student_data1 = student_data2
    | null (tail student_data1) = (head student_data1):student_data2
    | otherwise = myreversehelper (tail student_data1) ((head student_data1):student_data2)

myreverse_pm :: Eq a => [a] -> [a]
myreverse_pm student_data = myreversehelper_pm student_data []

myreversehelper_pm :: Eq a => [a] -> [a] -> [a]  
myreversehelper_pm [] student_data = student_data
myreversehelper_pm (x:[]) student_data = x:student_data
myreversehelper_pm (x:xs) student_data = myreversehelper_pm xs (x:student_data)
---------------------------------------------------------------------------------------------------------------------------
-- Problem 6
myreplaceall :: Eq a => a -> a -> [a] -> [a]
myreplaceall replaceWith toReplace student_data
    | null student_data = []
    | head student_data == toReplace = replaceWith : myreplaceall replaceWith toReplace (tail student_data)
    | otherwise = (head student_data) : myreplaceall replaceWith toReplace (tail student_data)

myreplaceall_pm :: Eq a => a -> a -> [a] -> [a]
myreplaceall_pm _ _ [] = []
myreplaceall_pm a b (x:xs)
    | x == b = a : myreplaceall a b xs
    | otherwise = x : myreplaceall a b xs
---------------------------------------------------------------------------------------------------------------------------
-- Problem 7
myordered :: (Ord a) => [a] -> Bool
myordered student_data 
    | null student_data = True
    | null (tail student_data) = True
    | head student_data > (head (tail student_data)) = False
    | otherwise = myordered (tail student_data)

myordered_pm :: (Ord a) => [a] -> Bool
myordered_pm [] = True
myordered_pm (x:[]) = True
myordered_pm (x:xs) 
    | x > (head xs) = False
    | otherwise = myordered xs
---------------------------------------------------------------------------------------------------------------------------
-- Problem 8
-- Process: Call computefees -> determine_students -> calculate student based on type 
-- Have a function to determine type of student
-- Separate functions to calculate fees for each type of student (Degree_aid, Degree_no_aid, Senior, Certificate)

computefees :: String -> Int
computefees student_data
    | null student_data = 0
    | otherwise = determine_student (split_string ';' student_data)

-- Functions to determine what kind of student is being processed
determine_student :: [String] -> Int
determine_student student_data 
    | get_degree_status student_data == "Y" = determine_aid_student student_data
    | otherwise = determine_nodegree_student student_data

determine_aid_student :: [String] -> Int
determine_aid_student student_data
    | get_aid_status student_data == "Y" = calculate_degree_finaid student_data
    | otherwise = calculate_degree_no_aid student_data

determine_nodegree_student :: [String] -> Int
determine_nodegree_student student_data
    | get_nodegree_status student_data == "S" = calculate_senior student_data
    | otherwise = calculate_cert_student student_data

-- Calculation Functions
calculate_degree_finaid :: [String] -> Int
calculate_degree_finaid student_data 
    | (get_credits student_data) >= 12 && 3450 <= (get_aid_amount student_data) = 0
    | (get_credits student_data) >= 12 = 3450 - (get_aid_amount student_data)
    | otherwise = ((get_credits student_data) * 275) + 150 - (get_aid_amount student_data)

calculate_degree_no_aid :: [String] -> Int
calculate_degree_no_aid student_data
    | (get_credits student_data) >= 12 = 3450
    | otherwise = ((get_credits student_data) * 275) + 150

calculate_senior :: [String] -> Int
calculate_senior student_data
    | get_credits student_data > 6 = 100 + 50 * ((get_credits student_data) - 6)
    | otherwise = 100

calculate_cert_student :: [String] -> Int
calculate_cert_student student_data = 700 + 300 * (get_credits student_data)

-- Get Student Data Functions
get_credits :: [String] -> Int
get_credits student_data = string_to_int (get_element_by_index 4 student_data)

get_aid_amount :: [String] -> Int
get_aid_amount student_data = string_to_int (get_element_by_index 9 student_data)

get_degree_status :: [String] -> String
get_degree_status student_data = get_element_by_index 5 student_data

get_aid_status :: [String] -> String
get_aid_status student_data = get_element_by_index 8 student_data

get_nodegree_status :: [String] -> String
get_nodegree_status student_data = get_element_by_index 6 student_data

---------------------------------------------------------------------------------------------------------------------------
-- Useful helper functions to process strings and convert string to int

string_to_int :: String -> Int -- function obtained from https://www.tutorialspoint.com/haskell-program-to-convert-the-string-into-an-integer
string_to_int s = read s

split_string :: Char -> String -> [String]
split_string _ [] = []
split_string delimiter string = split_string_helper delimiter string []

split_string_helper :: Char -> String -> String -> [String]
split_string_helper _ [] build_list = [build_list] -- Base case: the input string is empty then it has been processed or was passed empty
split_string_helper delimiter (x:xs) build_list 
    | x == delimiter = build_list : split_string_helper delimiter xs [] -- delimiter encountered, append built string to result of recursion
    | otherwise = split_string_helper delimiter xs (build_list ++ [x]) -- add chars together to build str if the delimiter is not encountered, recurse

get_element_by_index :: Int -> [String] -> String
get_element_by_index 0 (x:_) = x
get_element_by_index n (_:xs) = get_element_by_index (n-1) xs
---------------------------------------------------------------------------------------------------------------------------
