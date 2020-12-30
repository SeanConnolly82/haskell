import Data.Char
import System.IO
import System.Environment

main = do
    [f] <- getArgs
    contents <- readFile f
    let text_freq = freq_letter contents
    if eucl_dist text_freq eng_freq < eucl_dist text_freq pt_freq then do
        print "This text is in English"
    else
        print "This text is in Portuguese"

count :: Char -> String -> Float
count c s = sum [ 1 | character <- s, c == toLower character]

float_length_a_z :: String -> Float
float_length_a_z str = sum [ 1 | character <- str, elem (toLower character) ['a'..'z']]

eucl_dist :: [Float] -> [Float] -> Float
eucl_dist as bs = sqrt(sum[(a - b) * (a - b) | (a,b) <- zip as bs])

-- This version of the letter frequency will return a list with a frequency for each char 'a' to 'z'
freq_letter :: String -> [Float]
freq_letter str = [count letter str / float_length_a_z str | letter <- ['a'..'z']]

eng_freq :: [Float]
eng_freq = [8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07]

pt_freq :: [Float]
pt_freq = [12.21, 1.01, 3.35, 4.21, 13.19, 1.07, 1.08, 1.22, 5.49, 0.30, 0.13, 3.00, 5.07, 5.02, 10.22, 3.01, 1.10, 6.73, 7.35, 5.07, 4.46, 1.72, 0.05, 0.28, 0.04, 0.45]
