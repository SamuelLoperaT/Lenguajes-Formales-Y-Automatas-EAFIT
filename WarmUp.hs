import Test.QuickCheck
import Numeric.Natural

-- "Official" implementation for TESTING purpuses --

fac :: Natural -> Natural
fac n = product [1..n]

----------------------------------------------------

----------------------------------------------------

--facRight uses the Senior Haskell Programmer Implementation from http://www.willamette.edu/~fruehr/haskell/evolution.html
-- Uses Right Biased Trees to apply the "*" Function to the list from 1 to n
-- this means that the function is applied to the right of each element, like 1*(2*(3*4))) = fac1 4
facRight :: Natural -> Natural
facRight n = foldr (*) 1 [1..n]

----------------------------------------------------

--recFac uses a recursive implementation / definition of the factorial using pattern matching 
-- this means it recursively operates the definition n*(n-1)! = n! until reaching the case of n = 0 which is 1
-- this is similar to the Another junior Haskell programmer implementation from http://www.willamette.edu/~fruehr/haskell/evolution.html
recFac :: Natural -> Natural
recFac 0 = 1
recFac n = n*recFac(n-1)

----------------------------------------------------

--facIf uses a naïve implementation of the factorial, as found in the Freshman Haskell Programmer implementation from http://www.willamette.edu/~fruehr/haskell/evolution.html
-- force checks if n equals to 0, in which case returns 1
-- in other case it returns the recursive case n*(n-1)! = n!, until reaching the n = 0 case
facIf :: Natural -> Natural
facIf n = if n == 0 
           then 1
           else n * fac (n-1)

----------------------------------------------------

--facScan uses the function scanl (wich functions similarly to foldr, but returns a list with all the results of applying the function to the left)
-- then, it access the n elementh of the facs list, which corresponds to the factorial of the n number
-- this equals to the Memoizing Haskell Programmer (takes Ginko Biloba daily) Implementation from http://www.willamette.edu/~fruehr/haskell/evolution.html
facs = scanl (*) 1 [1..]

facScan :: Int -> Integer --weird type declaration due to issues with indexing :P
facScan n = facs !! n

----------------------------------------------------

--facFree uses a Point Free approach to the factorial as in the Pointless (ahem) “Points-free” Haskell programmer implementation from http://www.willamette.edu/~fruehr/haskell/evolution.html
-- this means that the argument is NOT explicitly expressed in the function, and is replaced in this case by a dot(.)
-- the multiplication is used into the list generated with the enumFromTo replacing the [1..n] syntax due to the dot being problematic in a point free approach, the operation itself is identical to facRight one (with foldr)
-- my understanding of points free is limited, i got it from a stackoverflow thread, this one to be precise https://stackoverflow.com/questions/944446/what-is-point-free-style-in-functional-programming

facFree :: Integer -> Integer
facFree = foldr (*) 1 . enumFromTo 1

----------------------------------------------------
