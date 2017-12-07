-- natural sorting to human read

import Data.List
import Data.Char
import Data.String.Utils
import Data.List.Utils
import Data.Function (on)

-- TODO gerar output com o valor original organizado e nao somente o novo valor organizado

print1Rule = do
                putStrLn "# Ignoring leading spaces \n"
                printBlockOfMessages sample1Rule ignoringStartEndSpaces --TODO devolver ao conteudo da lista original
                putStrLn "\n # Ignoring multiple adjacent spaces (m.a.s) \n"
                printBlockOfMessages sample2Rule ignoringMultipleAdjacentSpaces
                putStrLn "\n # Equivalent whitespace characters \n"
                printBlockOfMessages sample3Rule ignoringMultipleAdjacentSpaces
                putStrLn "\n # Case Indepenent sorts \n"
                printBlockOfMessages sample4Rule caseIndependent
                putStrLn "\n # Numeric fields as numerics \n"
                printBlockOfMessages sample5Rule numericFieldsAsNumbers
                putStrLn "\n # Title sorts \n"
                printBlockOfMessages sample6Rule removeLeadCommonWords

printMessage message content = do
                 putStrLn message
                 mapM_ print content

printBlockOfMessages list function = do
      printMessage "Text strings:" list
      printMessage "Normally sorted:" (sort list)
      printMessage "Naturally sorted:" (sortListWith list function)

-- function to execute all sorts
sortListWith l f = sort $ f l

-- 1. Ignore leading, trailing and multiple adjacent spaces

 -- Ignoring leading spaces
sample1Rule = ["ignore leading spaces: 2-2", " ignore leading spaces: 2-1", "  ignore leading spaces: 2+0",  "   ignore leading spaces: 2+1"]

-- receive a String and remove all spaces from the start and end of that String, a String is considered an List os Char
-- ex: "  a string " = "a string"
ignoringStartEndSpaces :: [String] -> [String]
ignoringStartEndSpaces = map strip

-- Ignoring multiple adjacent spaces and Equivalent whitespace characters
sample2Rule = ["ignore m.a.s spaces: 2-2", "ignore m.a.s  spaces: 2-1", "ignore m.a.s   spaces: 2+0", "ignore m.a.s    spaces: 2+1"]

ignoringMultipleAdjacentSpaces :: [String] -> [String]
ignoringMultipleAdjacentSpaces = map (unwords . words)

-- 2. Equivalent whitespace characters
sample3Rule = ["Equiv. spaces: 3-3", "Equiv.\rspaces: 3-2", "Equiv.\x0cspaces: 3-1", "Equiv.\x0bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2"]

-- 3. Case independent sort
sample4Rule = ["cASE INDEPENENT: 3-2", "caSE INDEPENENT: 3-1", "casE INDEPENENT: 3+0", "case INDEPENENT: 3+1"]

-- lower case of an entire String
-- ex "SomeCAse" = "somecase"
caseIndependent :: [String] -> [String]
caseIndependent = map (map toLower)

-- 4. Numeric fields as numerics (deals with up to 20 digits) 
sample5Rule = ["foo100bar99baz0.txt", "foo100bar10baz0.txt", "foo1000bar99baz10.txt", "foo1000bar99baz9.txt"]
sample5Rule' = ["foo3bar99baz2.txt", "foo2bar99baz3.txt", "foo1bar99baz4.txt", "foo4bar99baz1.txt"]

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

-- join a list of numbers into a single number
-- ex [4,2] = 42
joiner :: [Int] -> Int
joiner = read . concatMap show

-- 5. Title sort
sample6Rule = ["The Wind in the Willows", "The 40th step more", "The 39 steps", "Wanda"]

removeLeadCommonWords l = map removeLeadCommonWord $ splitList l

splitList = map words
removeLeadCommonWord a = unwords $ if f a commonWords then tail a else a 
                        where f l1 = elem (map toLower (head l1))
                              commonWords = ["the","a","an","of"]

-- 6. Equivalent accented characters (and case)
sample7Rule = ["Equiv. ý accents: 2-2", "Equiv. Ý accents: 2-1", "Equiv. y accents: 2+0", "Equiv. Y accents: 2+1"]
-- the normal sort function already has this feature

sample8Rule = ["Ĳ ligatured ij","no ligature"]
-- expected ["Ĳ ligatured ij","no ligature"]

sample9Rule = ["Start with an ʒ: 2-2", "Start with an ſ: 2-1", "Start with an ß: 2+0", "Start with an s: 2+1"]
-- expected 'Start with an s: 2+1' 'Start with an ſ: 2-1' 'Start with an ʒ: 2-2' 'Start with an ß: 2+0'

replacements = [("ß", "ss"), ("ſ", "s"), ("ʒ", "s")]
--map fst replacements retorna todas as chaves
