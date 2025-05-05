import Data.List


kadane :: [Integer] -> ( Integer , Int , Int )
kadane xs = ( sum , sIndex , eIndex ) where
	    ( sum , _ , _ , _ , sIndex , eIndex ) = 
		foldl ( \ ( mSum , cSum , sInd , eInd , sFin , eFin ) x -> if ( cSum + x > mSum ) then ( cSum + x , cSum + x , sInd , eInd + 1 , sInd , eInd + 1 ) 
									    else  
									      if ( cSum + x < 0 ) then ( mSum , 0 , eInd + 1 , eInd + 1 , sFin , eFin ) 
									       else ( mSum , cSum + x , sInd , eInd + 1, sFin , eFin  )  ) ( 0 , 0 , 0 , 0 , 0 , 0 ) xs
