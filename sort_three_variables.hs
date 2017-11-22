-- Sort three variables
-- Sort the values of three variables (X, Y, and Z) that contain any value (numbers and/or literals).

-- examples:
-- given     x = 'lions, tigers, and'
--           y = 'bears, oh my!'
--           z = '(from the "Wizard of OZ")'
-- expected:
--           x =  '(from the "Wizard of OZ")'
--           y =  'bears, oh my!'
--           z =  'lions, tigers, and'

import Data.List

sortThree :: (Ord a) => a -> a -> a -> [a]
sortThree x y z = sort (threeVariablesToList x y z)

threeVariablesToList :: a -> a -> a -> [a]
threeVariablesToList variable1 variable2 variable3 = [variable1, variable2, variable3]

test1 = showVariables l
            where x = "lions, tigers, and"
                  y = "bears, oh my!"
                  z = "(from the 'Wizard of OZ')"
                  l = sortThree x y z                

                  --TODO solucao falha para numeros
--showVariables :: [a] -> IO ()
showVariables [x,y,z] = print ("X = " ++ x ++ " Y = " ++ y ++ " Z = " ++ z)

-- direct solution with do notation

solution2 = do
      let x = "lions, tigers, and"
      putStrLn ("X = " ++ x)
      let y = "bears, oh my!"
      putStrLn ("Y = " ++ y)
      let z = "(from the 'Wizard of OZ')"
      putStrLn ("Z = " ++ z)
      let listSorted = sort [x,y,z]
      let x = head listSorted
      putStrLn (" ------------- ")
      putStrLn ("X = " ++ x)
      let y = listSorted !! 1
      putStrLn ("Y = " ++ y)
      let z = listSorted !! 2
      putStrLn ("Z = " ++ z)
