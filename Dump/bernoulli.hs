-- Fast computation of Bernoulli numbers B_n
-- http://mathworld.wolfram.com/BernoulliNumber.html
--
-- http://www.haskell.org/pipermail/haskell-cafe/2003-March/004063.html
-- http://www.haskell.org/pipermail/haskell-cafe/2003-March/004065.html
-- http://www.haskell.org/pipermail/haskell-cafe/2003-March/004075.html

module Bernoulli where

import Ratio


-- The 2D table of pre-computed powers = [[r^n | r<-[2..]] | n<-[1..]]
-- Thanks to lazy evaluation, the table is automatically `sized' as
-- needed. There is no need in any configuration parameters
powers = [2..] : map (zipWith (*) (head powers)) powers

-- the table of alternating sign powers = [[(-1)^r * r^n | r<-[2..]] | n<-1..]
-- Again, the table is sized as needed, with no configuration
neg_powers = 
  map (zipWith (\n x -> if n then x else -x) (iterate not True)) powers

-- Pre-computed Pascal triangle (again, automatically sized)
pascal:: [[Integer]]
pascal = [1,2,1] : map (\line -> zipWith (+) (line++[0]) (0:line)) pascal


-- A slow algorithm: an optimized version of the code by Damien R. Sullivan
-- It is based on the equality: Sum[ (n; k) B_k | k=0..n-1] = 0
b' 0 = fromIntegral 1
b' 2 = 1%6
b' n = -(sumbn n)/(fromIntegral (n+1))

sumbn:: Int -> Rational
sumbn n = 1 - (fromIntegral (n+1)%(fromIntegral 2)) + 
    sum [ (b' i) * fromIntegral(comb (n+1) i) | i  <- [2,4 .. n-1] ]
  where
  comb n i = pascal!!(n-2)!!i  -- Binomial coefficient

-- B_(22) = (854513)/(138)

-- The fast algorithm: it is faster than the above by at least 3 orders
-- of magnitude. It is based on the classical double-sum formula
-- over binomial coefficients
-- see the Mathworld page cited above.
-- The internal sum below computes with integers rather than rationals
bernoulli 0 = 1
bernoulli 1 = -(1%2)	
bernoulli n | odd n = 0
bernoulli n = 
   (-1)%2 
     + sum [ fromIntegral ((sum $ zipWith (*) powers (tail $ tail combs)) - 
                            fromIntegral k) %
             fromIntegral (k+1)
     | (k,combs)<- zip [2..n] pascal]
  where powers = (neg_powers!!(n-1))


-- To check that the denominator of the computed Bernoulli number is correct,
-- we use von Staudt-Clausen theorem
-- denom(B_2k) = 
--   product [ p | p <- takeWhile (< 2k+1) primes, 2k `rem` (p-1) == 0] 

primes       = 2:map head (iterate sieve [3,5..])
sieve (p:xs) = [ x | x<-xs, x `rem` p /= 0 ]

b_denom twok 
  = product [ p | p <- takeWhile (<= twok1) primes, 
                  twok `rem` (p-1) == 0]
  where twok1 = twok + 1

-- denom(B_n)==n for 1806, but for no other n (Kellner 2005)


{-
"Bernoulli of 82 is "
 1677014149185145836823154509786269900207736027570253414881613 % 498
"Bernoulli of 2000 is "
(-6773 ...4145 more digits.. 117) % 2338224387510
"Bernoulli of 3000 is "
(-2891939216292500 ... 6740 more digits.. 179981) % 12072109463901626300591430
-}

