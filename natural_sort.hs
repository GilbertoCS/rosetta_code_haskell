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
-- expected ['The 39 steps', 'The 40th step more', 'Wanda', 'The Wind in the Willows']

removeLeadCommonWords l = map removeLeadCommonWord $ breakList l

--breakList :: [[Char]] -> [[[Char]]]
breakList = map words
--removeLeadCommonWord :: [[String]] -> [String] -> String
removeLeadCommonWord a = unwords $ if f a commonWords then tail a else a 
                        where f l1 = elem (map toLower (head l1))
                              commonWords = ["the","a","an","of"]

-- 6. Equivalent accented characters (and case)
sample6Rule = ["Equiv. ý accents: 2-2", "Equiv. Ý accents: 2-1", "Equiv. y accents: 2+0", "Equiv. Y accents: 2+1"]
-- the normal sort function already has this feature

sample7Rule = ["Ĳ ligatured ij","no ligature"]
-- expected ["Ĳ ligatured ij","no ligature"]

sample8Rule = ["Start with an ʒ: 2-2", "Start with an ſ: 2-1", "Start with an ß: 2+0", "Start with an s: 2+1"]
-- expected 'Start with an s: 2+1' 'Start with an ſ: 2-1' 'Start with an ʒ: 2-2' 'Start with an ß: 2+0'

replacements = [("ß", "ss"), ("ſ", "s"), ("ʒ", "s")]
--map fst replacements retorna todas as chaves

{-- codigo phyton
 # Title
    words = s.split()
    if len(words) > 1 and words[0] in commonleaders:
        s = ' '.join( words[1:])
    # accent and ligatures
    s = ''.join(splitchar(c) for c in s)
    # Replacements (single char replaced by one or more)
    s = ''.join( replacements.get(ch, ch) for ch in s )
--}