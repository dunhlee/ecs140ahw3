-- Author: Dunh Adam Lee
-- Date: 07/18/2024
-- ECS 140A HW3

-------------------------------------------------------------------------------------------------------------------------
-- Problem 1 
myremoveduplicates :: Eq a => [a] -> [a] 
myremoveduplicates lst
    | null lst = []
    | head lst `elem` tail lst = myremoveduplicates (tail lst)
    | otherwise = (head lst) : myremoveduplicates (tail lst)

myremoveduplicates_pm :: Eq a => [a] -> [a]
myremoveduplicates_pm [] = []
myremoveduplicates_pm (x:xs)
    | x `elem` xs = myremoveduplicates xs
    | otherwise = x : myremoveduplicates xs
-------------------------------------------------------------------------------------------------------------------------
-- Problem 2
myintersection :: Eq a => [a] -> [a] -> [a]
myintersection lst1 lst2
    | null lst1 = []
    | null lst2 = []
    | head lst1 `elem` lst2 = (head lst1) : myintersection (tail lst1) lst2
    | otherwise = myintersection (tail lst1) lst2

myintersection_pm :: Eq a => [a] -> [a] -> [a]
myintersection_pm _ [] = []
myintersection_pm [] _ = []
myintersection_pm (x:xs) (y:ys)
    | x `elem` y:ys = myintersection xs (y:ys)
    | otherwise = myintersection xs (y:ys)
-------------------------------------------------------------------------------------------------------------------------
-- Problem 3
mynthtail :: Eq a => Int -> [a] -> [a]
mynthtail n lst
    | null lst = []
    | n == 0 = lst
    | otherwise = mynthtail (n - 1) (tail lst)

mynthtail_pm :: Eq a => Int -> [a] -> [a]
mynthtail_pm 0 (x:xs) = x:xs
mynthtail_pm _ [] = []
mynthtail_pm n (x:xs) = mynthtail (n - 1) xs
-------------------------------------------------------------------------------------------------------------------------
-- Problem 4
mylast :: Eq a => [a] -> [a]
mylast lst
    | null lst = [] -- base case: return an empty list if input is empty
    | null (tail lst) = [head lst] -- base case: return the first element if tail is empty
    | otherwise = mylast (tail lst) -- recursive call: run mylast again with the tail

mylast_pm :: Eq a => [a] -> [a]
mylast_pm [] = []
mylast_pm (x:[]) = [x]
mylast_pm (x:xs) = mylast xs
-------------------------------------------------------------------------------------------------------------------------
-- Problem 5
myreverse :: Eq a => [a] -> [a]
myreverse lst = myreversehelper lst []

myreversehelper :: Eq a => [a] -> [a] -> [a]
myreversehelper lst1 lst2
    | null lst1 = lst2
    | null (tail lst1) = (head lst1):lst2
    | otherwise = myreversehelper (tail lst1) ((head lst1):lst2)

myreverse_pm :: Eq a => [a] -> [a]
myreverse_pm lst = myreversehelper_pm lst []

myreversehelper_pm :: Eq a => [a] -> [a] -> [a]  
myreversehelper_pm [] lst = lst
myreversehelper_pm (x:[]) lst = x:lst
myreversehelper_pm (x:xs) lst = myreversehelper_pm xs (x:lst)
-------------------------------------------------------------------------------------------------------------------------
-- Problem 6
myreplaceall :: Eq a => a -> a -> [a] -> [a]
myreplaceall replaceWith toReplace lst
    | null lst = []
    | head lst == toReplace = replaceWith : myreplaceall replaceWith toReplace (tail lst)
    | otherwise = (head lst) : myreplaceall replaceWith toReplace (tail lst)

myreplaceall_pm :: Eq a => a -> a -> [a] -> [a]
myreplaceall_pm _ _ [] = []
myreplaceall_pm a b (x:xs)
    | x == b = a : myreplaceall a b xs
    | otherwise = x : myreplaceall a b xs
-------------------------------------------------------------------------------------------------------------------------
-- Problem 7
myordered :: (Ord a) => [a] -> Bool
myordered lst 
    | null lst = True
    | null (tail lst) = True
    | head lst > (head (tail lst)) = False
    | otherwise = myordered (tail lst)

myordered_pm :: (Ord a) => [a] -> Bool
myordered_pm [] = True
myordered_pm (x:[]) = True
myordered_pm (x:xs) 
    | x > (head xs) = False
    | otherwise = myordered xs
-------------------------------------------------------------------------------------------------------------------------
-- Problem 8
-- Process: Call computeFees -> determine_students -> calculate student based on type 
-- Have a function to determine type of student
-- Separate functions to calculate fees for each type of student (Degree_aid, Degree_no_aid, Senior, Certificate)

computeFees :: String -> Int
computeFees lst 
    | null lst = 0
    | otherwise = determine_student (split_string ';' lst)

-- Functions to determine what kind of student is being processed
determine_student :: [String] -> Int
determine_student lst 
    | get_degree_status lst == "Y" = determine_aid_student lst
    | otherwise = determine_nodegree_student lst

determine_aid_student :: [String] -> Int
determine_aid_student lst
    | get_aid_status lst == "Y" = calculate_degree_finaid lst
    | otherwise = calculate_degree_no_aid lst

determine_nodegree_student :: [String] -> Int
determine_nodegree_student lst
    | get_nodegree_status lst == "S" = calculate_senior lst
    | otherwise = calculate_cert_student lst

-- Calculation Functions
calculate_degree_finaid :: [String] -> Int
calculate_degree_finaid lst 
    | (get_credits lst) >= 12 && 3450 <= (get_aid_amount lst) = 0
    | (get_credits lst) >= 12 = 3450 - (get_aid_amount lst)
    | otherwise = ((get_credits lst) * 275) + 150 - (get_aid_amount lst)

calculate_degree_no_aid :: [String] -> Int
calculate_degree_no_aid lst
    | (get_credits lst) >= 12 = 3450
    | otherwise = ((get_credits lst) * 275) + 150

calculate_senior :: [String] -> Int
calculate_senior lst
    | get_credits lst > 6 = 100 + 50 * ((get_credits lst) - 6)
    | otherwise = 100

calculate_cert_student :: [String] -> Int
calculate_cert_student lst = 700 + 300 * (get_credits lst)

-- Get Student Data Functions
get_credits :: [String] -> Int
get_credits lst = string_to_int (get_element_by_index 4 lst)

get_aid_amount :: [String] -> Int
get_aid_amount lst = string_to_int (get_element_by_index 9 lst)

get_degree_status :: [String] -> String
get_degree_status lst = get_element_by_index 5 lst

get_aid_status :: [String] -> String
get_aid_status lst = get_element_by_index 8 lst

get_nodegree_status :: [String] -> String
get_nodegree_status lst = get_element_by_index 6 lst

-------------------------------------------------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------------------------------------------------
