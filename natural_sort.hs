-- natural sorting to human read

import Data.List
import Data.Char
import Data.String.Utils
import Data.Function (on)

-- TODO gerar output com o valor original organizadoe nao somente o novo valor organizado

-- function to execute all sorts
sortListWith l f = sort $ f l

-- 1. Ignore leading, trailing and multiple adjacent spaces

 -- Ignoring leading spaces
sampleFirstRule = ["ignore leading spaces: 2-2", " ignore leading spaces: 2-1", "  ignore leading spaces: 2+0",  "   ignore leading spaces: 2+1"]

-- receive a String and remove all spaces from the start and end of that String, a String is considered an List os Char
-- ex: "  a string " = "a string"
ignoringStartEndSpaces :: [String] -> [String]
ignoringStartEndSpaces = map strip

-- Ignoring multiple adjacent spaces and Equivalent whitespace characters
sampleSecondRule = ["ignore m.a.s spaces: 2-2", "ignore m.a.s  spaces: 2-1", "ignore m.a.s   spaces: 2+0", "ignore m.a.s    spaces: 2+1"]

ignoringMultipleAdjacentSpaces :: [String] -> [String]
ignoringMultipleAdjacentSpaces = map (unwords . words)

-- Equivalent whitespace characters
sampleThirdRule = ["Equiv. spaces: 3-3", "Equiv.\rspaces: 3-2", "Equiv.\x0cspaces: 3-1", "Equiv.\x0bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2"]

--Case independent sort
sampleFourthRule = ["cASE INDEPENENT: 3-2", "caSE INDEPENENT: 3-1", "casE INDEPENENT: 3+0", "case INDEPENENT: 3+1"]

caseIndependent :: [String] -> [String]
caseIndependent = map (map toLower)

-- Numeric fields as numerics (deals with up to 20 digits) 
sampleFifthRule = ["foo100bar99baz0.txt", "foo100bar10baz0.txt", "foo1000bar99baz10.txt", "foo1000bar99baz9.txt"]

-- expected   ['foo100bar10baz0.txt',  'foo100bar99baz0.txt', 'foo1000bar99baz9.txt', 'foo1000bar99baz10.txt']


-- let res =  groupBy ((==) `on` isNumber ) "string"
-- map (filter isNumber) res
-- usar join depois

{--
val r3 = Regex("""\s""")  // \s represents any whitespace characterval r5 = Regex("""\d+""")
 
 # Numeric sections as numerics
    s = [ int("".join(g)) if isinteger else "".join(g)
          for isinteger,g in groupby(s, lambda x: x in decdigits)]
 
/** Numeric fields as numerics (deals with up to 20 digits) */ 
fun selector5(s: String) = r5.replace(s) { it.value.padStart(20, '0') }
--}
