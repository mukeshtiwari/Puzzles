import Data.Function


factOpen :: ( Integer -> Integer ) -> Integer -> Integer
factOpen recur 0 = 1 
factOpen recur n = n * recur ( pred n )

fact :: Integer -> Integer
fact = fix factOpen


--factM :: Integer -> Integer
--factM = memoFix factOpen
