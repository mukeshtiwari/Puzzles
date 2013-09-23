import Data.SBV



inversePolynomial ::  [ Int ] -> [ Int ] ->  SWord16 
inversePolynomial poly reducer =  inversePoly  reducer' rem' first' second' depth'  where
			poly' =  polynomial poly :: SWord16
			reducer' =  polynomial reducer :: SWord16  
			first' =  0 :: SWord16
			second' = 1 :: SWord16
			depth' = 20 :: SWord16
			( quot', rem' ) = pDivMod poly' reducer'
                        
inversePoly :: SWord16 -> SWord16 -> SWord16 -> SWord16  ->  SWord16 -> SWord16
inversePoly  reducer'' poly'' first'' second'' depth'' =  
	       ite ( depth'' .== 0 ||| rem'' .== ( 0 :: SWord16 ) ) (  second'' )  ( inversePoly  poly'' rem'' second'' ret ( depth'' - 1 ) )  where
		                   ( quot'', rem'' ) = pDivMod reducer'' poly''
				   ret =  pAdd ( pMult ( quot'', second'', [] ) ) first''	

isInversePolyCorrect :: SWord16 -> SWord16 -> SBool
isInversePolyCorrect poly reducer = inversePoly reducer ( inversePoly reducer rem first second depth ) first second depth .== rem where
			( quot, rem ) = pDivMod  poly reducer
			first = 0 :: SWord16
			second = 1 :: SWord16 
			depth = 20 :: SWord16

--main :: IO ()
--main = ( prove  $ forAll ["x"] $ \x ->  pMult ( inversePoly ( 283 :: SWord16 ) x ( 0 :: SWord16 ) ( 1 :: SWord16 ) ( 50 :: SWord16 ) , x, [ 8,4,3,1,0] ) .== ( 1 :: SWord16 ) ) >> return ()
