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

-- 2. Equivalent whitespace characters
sampleThirdRule = ["Equiv. spaces: 3-3", "Equiv.\rspaces: 3-2", "Equiv.\x0cspaces: 3-1", "Equiv.\x0bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2"]

-- 3. Case independent sort
sampleFourthRule = ["cASE INDEPENENT: 3-2", "caSE INDEPENENT: 3-1", "casE INDEPENENT: 3+0", "case INDEPENENT: 3+1"]

-- lower case of an entire String
-- ex "SomeCAse" = "somecase"
caseIndependent :: [String] -> [String]
caseIndependent = map (map toLower)

-- join a list of numbers into a single number
-- ex [4,2] = 42
joiner :: [Int] -> Int
joiner = read . concatMap show

-- 4. Numeric fields as numerics (deals with up to 20 digits) 
sampleFifthRule = ["foo100bar99baz0.txt", "foo100bar10baz0.txt", "foo1000bar99baz10.txt", "foo1000bar99baz9.txt"]
-- expected   ['foo100bar10baz0.txt',  'foo100bar99baz0.txt', 'foo1000bar99baz9.txt', 'foo1000bar99baz10.txt']

numericFieldsAsNumbers :: [String] -> [[Int]]
numericFieldsAsNumbers = map findOnlyNumerics

findOnlyNumerics :: String -> [Int]
findOnlyNumerics s = convertDigitAsStringToInt $ makeListOfDigitsAsString $ extractDigitsAsString s
extractDigitsAsString :: String -> [String]
extractDigitsAsString s = map (filter isNumber) $ groupBy ((==) `on` isNumber ) s
makeListOfDigitsAsString :: [String] -> [String]
makeListOfDigitsAsString l = tail $ nub l
convertDigitAsStringToInt :: [String] -> [Int]
convertDigitAsStringToInt = map (joiner . map  digitToInt)

-- 5. Title sort
sampleSixThyRule = ["The Wind in the Willows", "The 40th step more", "The 39 steps", "Wanda"]

common = ["the","a","an","of"]

-- let a = map words l
-- let b = caseIndependent (head a)
-- elem (head b) common

-- stripPrefix (common !! 0) (map toLower "The Wind in the Willows")