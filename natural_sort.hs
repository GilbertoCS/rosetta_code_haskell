-- natural sorting to human read

import Data.List
import Data.Char

-- TODO gerar output com o valor original organizadoe nao somente o novo valor organizado

-- 1. Ignore leading, trailing and multiple adjacent spaces

 -- Ignoring leading spaces
sampleFirstRule = ["ignore leading spaces: 2-2", " ignore leading spaces: 2-1", "  ignore leading spaces: 2+0",  "   ignore leading spaces: 2+1"]

removeLeadingSpaces :: String -> String
removeLeadingSpaces = dropWhile isSpace

sortIgnoringLeadingSpaces :: [String] -> [String]
sortIgnoringLeadingSpaces l = sort $ map removeLeadingSpaces l

-- Ignoring multiple adjacent spaces and Equivalent whitespace characters
sampleSecondRule = ["ignore m.a.s spaces: 2-2", "ignore m.a.s  spaces: 2-1", "ignore m.a.s   spaces: 2+0", "ignore m.a.s    spaces: 2+1"]

sortIgnoringMultipleAdjacentSpaces :: [String] -> [String]
sortIgnoringMultipleAdjacentSpaces l = sort $ map (unwords . words) l

-- Equivalent whitespace characters
sampleThirdRule = ["Equiv. spaces: 3-3", "Equiv.\rspaces: 3-2", "Equiv.\x0cspaces: 3-1", "Equiv.\x0bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2"]

sampleFourthRule = ["cASE INDEPENENT: 3-2", "caSE INDEPENENT: 3-1", "casE INDEPENENT: 3+0", "case INDEPENENT: 3+1"]
{--
val r3 = Regex("""\s""")  // \s represents any whitespace characterval r5 = Regex("""\d+""")
 

 
/** Case independent sort */
fun selector4(s: String) = s.toLowerCase()
 
/** Numeric fields as numerics (deals with up to 20 digits) */ 
fun selector5(s: String) = r5.replace(s) { it.value.padStart(20, '0') }
--}
