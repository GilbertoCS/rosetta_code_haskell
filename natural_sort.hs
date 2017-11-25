-- natural sorting to human read

import Data.List
import Data.Char
-- 1. Ignore leading, trailing and multiple adjacent spaces

{--
Text strings:
['ignore leading spaces: 2-2',
 ' ignore leading spaces: 2-1',
 '  ignore leading spaces: 2+0',
 '   ignore leading spaces: 2+1']
Normally sorted :
['   ignore leading spaces: 2+1',
 '  ignore leading spaces: 2+0',
 ' ignore leading spaces: 2-1',
 'ignore leading spaces: 2-2']
Naturally sorted:
['  ignore leading spaces: 2+0',
 '   ignore leading spaces: 2+1',
 ' ignore leading spaces: 2-1',
 'ignore leading spaces: 2-2']
 --}

 -- Ignoring leading spaces
sampleFirstRule = ["ignore leading spaces: 2-2", " ignore leading spaces: 2-1", "  ignore leading spaces: 2+0",  "   ignore leading spaces: 2+1"]

ignoringLeadingSpaces :: String -> String
ignoringLeadingSpaces = dropWhile isSpace

sortFirstRule :: [String] -> [String]
sortFirstRule l = sort $ map ignoringLeadingSpaces l
