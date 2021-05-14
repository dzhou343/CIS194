-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import Week1.HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

testToRevDigit :: (Integer, [Integer]) -> Bool 
testToRevDigit (n, d) = toRevDigits n == d

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool 
testDoubleEveryOther (inp, exp) = doubleEveryOther inp == exp

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (inp, exp) = sumDigits inp == exp

testLuhn :: (Integer, Bool) -> Bool 
testLuhn (inp,exp) = luhn inp == exp

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = [ Test "toRevDigit tests" testToRevDigit 
            [(123, [3,2,1]), (0, []), (320, [0,2,3])]
            ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = [Test "doubleEveryOther tests" testDoubleEveryOther 
            [([1,2,3,4], [1,4,3,8]), ([1,2],[1,4]), ([1],[1]), ([0,10,0,10,0,10], [0,20,0,20,0,20])]
            ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = [Test "sumDigit tests" testSumDigits
            [([1,2],3), ([1,2,3],6), ([0], 0)]
            ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = [Test "luhn tests" testLuhn
            [(79927398710, True)]
            ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = []

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
