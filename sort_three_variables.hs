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

-- test 1
x = "lions, tigers, and"
y = "bears, oh my!"
z = "(from the 'Wizard of OZ')"

sortThree :: (Ord a) => a -> a -> a -> [a]
sortThree x y z = sort (threeVariablesToList x y z)

threeVariablesToList :: a -> a-> a -> [a]
threeVariablesToList variable1 variable2 variable3 = [variable1, variable2, variable3]

-- show sortThree x y z
                                