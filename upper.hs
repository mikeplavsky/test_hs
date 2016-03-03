import Data.Char (toUpper)
-- main = interact (map toUpper . (++) "Your data:\n\n")
main = interact ((++) "Your data:\n\n" . (map toUpper))

z = 'a':[x | x <- z]
f = (++) "Yours: " . map toUpper

check = take 10 (f z)

z1 = "aaa" : [x | x <- z1]
x = unlines z1

g = filter (elem 'a') . lines
g1 = (take 10 . g ) x
g2 = (g . take 10) x
