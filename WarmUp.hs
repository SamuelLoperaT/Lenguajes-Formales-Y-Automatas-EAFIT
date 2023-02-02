import Test.QuickCheck
import Numeric.Natural

-- "Official" implementation for TESTING purpuses --

fac :: Natural -> Natural
fac n = product [1..n]

----------------------------------------------------

----------------------------------------------------

--fac1 uses the Senior Haskell Programmer Implementation from http://www.willamette.edu/~fruehr/haskell/evolution.html
-- Uses Right Biased Trees to apply the "*" Function to the list from 1 to n
-- this means that the function is applied to the right of each element, like 1*(2*(3*4))) = fac1 4
fac1 :: Natural -> Natural
fac1 n = foldr (*) 1 [1..n]

----------------------------------------------------

--fac2 uses a recursive implementation / definition of the factorial using pattern matching 
-- this means it recursively operates the definition n*(n-1)! = n! until reaching the case of n = 0 which is 1
-- this is similar to the Another junior Haskell programmer implementation from http://www.willamette.edu/~fruehr/haskell/evolution.html
fac2 :: Natural -> Natural
fac2 0 = 1
fac2 n = n*fac2(n-1)

----------------------------------------------------

--fac3 uses a naïve implementation of the factorial, as found in the Freshman Haskell Programmer implementation from http://www.willamette.edu/~fruehr/haskell/evolution.html
-- force checks if n equals to 0, in which case returns 1
-- in other case it returns the recursive case n*(n-1)! = n!, until reaching the n = 0 case
fac3 :: Natural -> Natural
fac3 n = if n == 0 
           then 1
           else n * fac (n-1)

----------------------------------------------------

--fac4 uses the function scanl (wich functions similarly to foldr, but returns a list with all the results of applying the function to the left)
-- then, it access the n elementh of the facs list, which corresponds to the factorial of the n number
-- this equals to the Memoizing Haskell Programmer (takes Ginko Biloba daily) Implementation from http://www.willamette.edu/~fruehr/haskell/evolution.html
facs = scanl (*) 1 [1..]

fac4 :: Natural -> Natural
fac4 n = fromInteger (facs !! (fromIntegral (toInteger n))) --Several conversions to match the type signature to Natrual -> Natural, probably there is a more optimal way of doing this, don't care :p

----------------------------------------------------

--fac5 uses a Point Free approach to the factorial as in the Pointless (ahem) “Points-free” Haskell programmer implementation from http://www.willamette.edu/~fruehr/haskell/evolution.html
-- this means that the argument is NOT explicitly expressed in the function, and is replaced in this case by a dot(.)
-- the multiplication is used into the list generated with the enumFromTo replacing the [1..n] syntax due to the dot being problematic in a point free approach, the operation itself is identical to fac1 one (with foldr)
-- my understanding of points free is limited, i got it from a stackoverflow thread, this one to be precise https://stackoverflow.com/questions/944446/what-is-point-free-style-in-functional-programming

fac5 :: Natural -> Natural
fac5 = foldr (*) 1 . enumFromTo 1


---------------------------------------------------

prop_fac :: [Natural -> Natural]-> Natural -> Bool
prop_fac 

main :: IO ()
main = quickCheck $
    prop_fac [fac1, fac2, fac3, fac4, fac5]