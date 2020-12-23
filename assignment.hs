import Data.Char
import System.IO
import System.Environment

-- Q1
is_square :: Int -> Bool
is_square n = elem n [ x*x | x <- [1..n], x*x<=n]

-- Q2
freq_letter_pc :: String -> [(Char, Float)]
freq_letter_pc str = [(letter, count letter str / float_length_a_z str) | letter <- ['a'..'z'], elem letter str]

count :: Char -> String -> Float
count c s = sum [ 1 | character <- s, c == toLower character]

float_length_a_z :: String -> Float
float_length_a_z str = sum [ 1 | character <- str, elem (toLower character) ['a'..'z']]

-- Q3
type CityId = Int
type CityName = String
type CountryName = String
type Population = Int
type CountryId = Int
type City = [(CityId, CityName, Population, CountryId)]
type Country = [(CountryId, CountryName)]

get_city_above :: Int -> [CityName]
get_city_above n = [ b | (a,b,c,d) <- cities, c >= n]

get_city :: CountryName -> [CityName]
get_city country_name = [ b | (a,b,c,d) <- cities, (e,f) <- countries, d == e, f == country_name]

num_city :: [(CountryName, Int)]
num_city = [(y,length(get_city y)) | (x,y) <- countries]

cities :: City
cities = [  (1,"Paris",7000000,1),
            (2,"London",8000000,2),
            (1,"Rome",3000000,3), 
            (1,"Edinburgh",500000,2),
            (1,"Florence",50000,3), 
            (1,"Venice",200000,3), 
            (1,"Lyon",1000000,1), 
            (1,"Milan",3000000,3), 
            (1,"Madrid",6000000,4),
            (1,"Barcelona",5000000,4)]

countries :: Country
countries = [(2,"UK"), (1,"France"), (3,"Italy"), (4,"Spain")]

-- Q4
eucl_dist :: [Float] -> [Float] -> Float
eucl_dist as bs = sqrt(sum[(a - b) * (a - b) | (a,b) <- zip as bs])

-- Q5
-- see get_lang.hs for command line version
get_lang :: String -> IO ()
get_lang f = do
    contents <- readFile f
    let text_freq = freq_letter contents
    if eucl_dist text_freq eng_freq < eucl_dist text_freq pt_freq then do
        print "This text is in English"
    else
        print "This text is in Portuguese"

-- This version of the letter frequency will return a list with a frequency for each char 'a' to 'z'
freq_letter :: String -> [Float]
freq_letter str = [count letter str / float_length_a_z str | letter <- ['a'..'z']]

eng_freq :: [Float]
eng_freq = [8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07]

pt_freq :: [Float]
pt_freq = [12.21, 1.01, 3.35, 4.21, 13.19, 1.07, 1.08, 1.22, 5.49, 0.30, 0.13, 3.00, 5.07, 5.02, 10.22, 3.01, 1.10, 6.73, 7.35, 5.07, 4.46, 1.72, 0.05, 0.28, 0.04, 0.45]

-- Q6
 
-- Q7

-- Q8

-- Q9
math_series :: (Float -> Float) -> Int -> Float
math_series f n 
    | n >= 0 = f (fromIntegral n)
    | otherwise = 0

pi_series :: Float -> Float
pi_series 0 = 0
pi_series k = ((-1) ** (k + 1)) * (4 / ((2 * k) - 1)) + pi_series (k - 1)

-- Q10
-- Call the function and pass in f
integral :: (Float -> Float) -> Float -> Float -> Int -> Float
integral func x1 x2 n 
    | n > 0 = sum [func x * ((x2 - x1) / fromIntegral n) | x <- [x1,x1 + ((x2 - x1) / fromIntegral n)..x2], x < x2]
    | otherwise = 0

f :: Float -> Float
f x = 0.5 * x


