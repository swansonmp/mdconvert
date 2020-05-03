-- mdconvert
-- Matthew Swanson

import Data.List (intercalate, elemIndices)
import Data.List.Split (splitOn)
import Data.Char (isSpace, isDigit)
import System.IO
import System.Environment

type Text = String
type Href = String
type Src  = String
type Type = String

data Tag = H1 [Tag]
         | H2 [Tag]
         | H3 [Tag]
         | H4 [Tag]
         | H5 [Tag]
         | H6 [Tag]
         | P [Tag]
         | Em [Tag]
         | Strong [Tag]
         | Code [Tag]
         | HR
         | Img Src
         | Video Src Type
         | Audio Src Type
         | Blockquote [Tag]
         | A Href Text
         | UL [Tag]
         | OL [Tag]
         | LI [Tag]
         | Table [Tag]
         | TR [Tag]
         | TD [Tag]
         | Text String
         | PHTML String
         | Out String
         | Err
            deriving Show

sr :: ([Tag], [Tag]) -> ([Tag], [Tag])
sr s = ([], [])

-- Stack and Stack operations
-----------------------------
type Stack = [String]

push :: String -> Stack -> Stack
push s stack = s : stack

pop :: Stack -> (String, Stack)
pop (top:stack) = (top, stack)
pop []          = ("", [])

empty :: Stack -> Bool
empty stack = stack == []
-----------------------------

notSyntaxChar:: Char -> Bool
notSyntaxChar s
    | s == '*'  = False
    | s == '_'  = False
    | s == '`'  = False 
    | s == '~'  = False 
    | s == '['  = False
    | otherwise = True

isSyntaxSymbol :: String -> Bool
isSyntaxSymbol s
    | s == "**" = True
    | s == "_"  = True
    | s == "`"  = True
    | s == "~~" = True
    | otherwise = False

getTagName :: String -> String
getTagName s
    | s == "**" = "strong"
    | s == "_"  = "em"
    | s == "`"  = "code"
    | s == "~~" = "strike"
    | otherwise = error "getTagName: Invalid input"

getHref :: String -> String
getHref       s = takeWhile ('"' /=) (drop ((indices !! 2) + 1) s)
                    where indices = elemIndices '"' s

getAnchorText :: String -> String
getAnchorText s = takeWhile ('"' /=) (drop ((indices !! 0) + 1) s)
                    where indices = elemIndices '"' s

parseAnchor :: String -> String
parseAnchor s = "<a href=\"" ++ (getHref s) ++ "\">" ++ (getAnchorText s) ++ "</a>"

splitOnInlineSyntax :: String -> [String]
splitOnInlineSyntax ""          = []
splitOnInlineSyntax ('_':s)     = "_"  : splitOnInlineSyntax s
splitOnInlineSyntax ('`':s)     = "`"  : splitOnInlineSyntax s
splitOnInlineSyntax ('*':'*':s) = "**" : splitOnInlineSyntax s
splitOnInlineSyntax ('~':'~':s) = "~~" : splitOnInlineSyntax s
splitOnInlineSyntax ('*':s) = ('*':words) : splitOnInlineSyntax rest
                                where (words, rest) = (takeWhile (notSyntaxChar) s, dropWhile (notSyntaxChar) s)
splitOnInlineSyntax ('~':s) = ('~':words) : splitOnInlineSyntax rest
                                where (words, rest) = (takeWhile (notSyntaxChar) s, dropWhile (notSyntaxChar) s)
splitOnInlineSyntax ('[':'[':s) = if rest /= ""
                                    then if length rest > 1 && (rest !! 1) == ']'
                                        then (parseAnchor words) : splitOnInlineSyntax (tail (tail rest))
                                        else "[[" : splitOnInlineSyntax s
                                    else "[[" : splitOnInlineSyntax s
                                            where (words, rest) = (takeWhile (']' /=) s, dropWhile (']' /=) s)
splitOnInlineSyntax ('[':s) = ('[':words) : splitOnInlineSyntax rest
                                where (words, rest) = (takeWhile (notSyntaxChar) s, dropWhile (notSyntaxChar) s)
splitOnInlineSyntax s       = words : splitOnInlineSyntax rest
                                where (words, rest) = (takeWhile (notSyntaxChar) s, dropWhile (notSyntaxChar) s)
                                    

readInlineHelper :: [String] -> Stack -> String
readInlineHelper (s:ss) stack = if s == top
                                    then "</" ++ (getTagName s) ++ ">" 
                                            ++ readInlineHelper ss newStack
                                    else if isSyntaxSymbol s
                                        then "<" ++ (getTagName s) ++ ">" 
                                                ++ readInlineHelper ss (push s stack)
                                        else s ++ readInlineHelper ss stack
                                            where (top, newStack) = pop stack
readInlineHelper []     stack = ""

hasInlineHTML :: String -> Stack -> Bool
hasInlineHTML ('<':s) stack = if not (empty stack)
                                  then False
                                  else hasInlineHTML s (push "<" stack)
hasInlineHTML ('>':s) stack = if empty stack
                                  then False
                                  else hasInlineHTML s newStack
                                      where (top, newStack) = pop stack
hasInlineHTML (c:s)   stack = hasInlineHTML s stack
hasInlineHTML []      stack = empty stack

readInline :: String -> [Tag]
readInline s = if hasInlineHTML s []
                   then [PHTML (replace "   \n" "<br>\n" (readInlineHelper (splitOnInlineSyntax s) []))]
                   else [PHTML (replace "   \n" "<br>\n" (readInlineHelper (splitOnInlineSyntax s') []))]
                       where s' = replace "<" "&lt;" (replace ">" "&gt;" s)

-- Replace Method
-- Credit: Santa Claus
-- https://stackoverflow.com/a/49261236
replace :: String -> String -> String -> String
replace from to = intercalate to . splitOn from

getSrc :: String -> String
getSrc s = init (drop 2 s)

parseImg :: String -> Tag
parseImg s = Img (getSrc s)

parseVideo :: String -> Tag
parseVideo s = Video src ((splitOn "." src) !! 1)
                where src = getSrc s

parseAudio :: String -> Tag
parseAudio s = Audio src ((splitOn "." src) !! 1)
                where src = getSrc s

parseUL :: String -> Tag
parseUL s = UL [LI [P (readInline (strip (tail (tail l))))] | l <- (lines s)]

parseOL :: String -> Tag
parseOL s = OL [LI [P (readInline (strip (tail (tail (tail l)))))] | l <- (lines s)]

parseTableRow :: String -> Tag
parseTableRow s = TR [TD [P (readInline d)] | d <- splitOn "," s]

parseTable :: String -> Tag
parseTable s = Table (tail (map parseTableRow (lines s)))

-- Converts a single string into a token, producing error on non-tokens
readBlock :: String -> Tag
readBlock ('#':'#':'#':'#':'#':'#':' ':cs) = H6 (readInline cs)
readBlock ('#':'#':'#':'#':'#':' ':cs)     = H5 (readInline cs)
readBlock ('#':'#':'#':'#':' ':cs)         = H4 (readInline cs)
readBlock ('#':'#':'#':' ':cs)             = H3 (readInline cs)
readBlock ('#':'#':' ':cs)                 = H2 (readInline cs)
readBlock ('#':' ':cs)                     = H1 (readInline cs)
readBlock ('-':'-':'-':cs) = HR
readBlock ('*':'*':'*':cs) = HR
readBlock ('_':'_':'_':cs) = HR
readBlock ('!':'(':cs) = parseImg ('!':'(':cs)
readBlock ('^':'(':cs) = parseVideo ('^':'(':cs)
readBlock ('@':'(':cs) = parseAudio ('@':'(':cs)
readBlock ('+':' ':cs) = parseTable cs
readBlock ('>':' ':cs) = Blockquote [P (readInline cs)]
readBlock ('*':' ':cs) = parseUL ('*':' ':cs)
readBlock s
    | len > 3 && isDigit (s !! 0) && (s !! 1) == '.' && (s !! 2) == ' ' = parseOL s
    | otherwise = P (readInline s)
        where len = length s

lexer :: String -> [Tag]
lexer s = map readBlock (preproc s)

-- Naive strip method
-- Credit: Eric Normand
-- https://stackoverflow.com/a/6270337
strip :: String -> String
strip = f . f
        where f = reverse . dropWhile isSpace

-- Takes in an input string and splits it into 'blocks'
-- A block is a contiguous group of text deliniated by two newlines
-- Additionally, a space is added before single newlines
preproc :: String -> [String]
preproc s = [strip (addSpacesBeforeNewline b) | b <- (splitOn "\n\n" s)]

-- Adds spaces before every newline character
addSpacesBeforeNewline :: String -> String
addSpacesBeforeNewline ('\n':cs) = " \n" ++ (addSpacesBeforeNewline cs)
addSpacesBeforeNewline (c:cs)    = c : (addSpacesBeforeNewline cs)
addSpacesBeforeNewline []        = ""

parser :: [Tag] -> Maybe String
parser ts = let (x,y) = sr (ts,[])
                in case y of
                    [Out s] -> Just s
                    [Err]      -> Nothing
                    _          -> Nothing                    

-- Converts a list of tags into an HTML String
toString :: [Tag] -> String
toString ((H1 s):ts) = "<h1>" ++ (toString s) ++ "</h1>\n" ++ (toString ts)
toString ((H2 s):ts) = "<h2>" ++ (toString s) ++ "</h2>\n" ++ (toString ts)
toString ((H3 s):ts) = "<h3>" ++ (toString s) ++ "</h3>\n" ++ (toString ts)
toString ((H4 s):ts) = "<h4>" ++ (toString s) ++ "</h4>\n" ++ (toString ts)
toString ((H5 s):ts) = "<h5>" ++ (toString s) ++ "</h5>\n" ++ (toString ts)
toString ((H6 s):ts) = "<h6>" ++ (toString s) ++ "</h6>\n" ++ (toString ts)
toString (HR:ts) = "<hr />\n" ++ (toString ts)
toString ((Img src):ts) = "<p><img src=\"" ++ src ++ "\"/></p>\n" 
                                    ++ (toString ts)
toString ((Video src type_):ts) = "<p><video width=\"320\" height=\"240\" controls>\n"
                                    ++ "\t<source src=\"" ++ src ++ "\" type=\"video/"
                                    ++ type_ ++ "\">\n\tVideo not supported.\n</video></p>\n"
                                    ++ (toString ts)
toString ((Audio src type_):ts) = "<p><audio controls>\n\t<source src=\"" ++ src
                                    ++ "\" type=\"audio/" ++ type_
                                    ++ "\">\n\tAudio not supported.\n</audio></p>\n"
                                    ++ (toString ts)
toString ((Blockquote t):ts) = "<blockquote>" ++ (toString t) ++ "</blockquote>\n" ++ (toString ts)
toString ((UL t):ts) = "<ul>\n" ++ (toString t) ++ "</ul>\n" ++ (toString ts)
toString ((OL t):ts) = "<ol>\n" ++ (toString t) ++ "</ol>\n" ++ (toString ts)
toString ((LI t):ts) = "\t<li>" ++ (toString t) ++ "</li>\n" ++ (toString ts)
toString ((Table t):ts) = "<table>\n" ++ (toString t) ++ "</table>\n" ++ (toString ts)
toString ((TR t):ts) = "<tr>\n" ++ (toString t) ++ "</tr>\n" ++ (toString ts)
toString ((TD t):ts) = "<td>\n" ++ (toString t) ++ "</td>\n" ++ (toString ts)
toString ((P t):ts) = "<p>" ++ (toString t) ++ "</p>\n" ++ (toString ts)
toString ((Text t):ts) = t
toString ((PHTML t):ts) = t ++ (toString ts)
toString (_:ts) = "" ++ (toString ts)
toString [] = ""

wrap :: String -> String -> String
wrap w s = "<" ++ w ++ ">\n" ++ s ++ "</" ++ w ++ ">\n"

convert :: String -> Maybe String
--convert s = parser (lexer s)
convert s = Just (wrap "html" (toString (lexer s)))

sconvert :: String -> String -> Maybe String
sconvert s style = Just (wrap "html" ((wrap "head" (wrap "style" style)) 
                       ++ (wrap "body" (toString (lexer s)))))

printUsage :: IO ()
printUsage = putStrLn "usage: mdconvert infile [outfile] [-s stylefile]"

-- Main function
main = do
    args <- getArgs
    if args == [] || head (head args) == '-'
        then do printUsage
        -- infile present
        else do
            let infile = head args
            inhandle <- openFile infile ReadMode
            incontents <- hGetContents inhandle
            if length args == 1
                -- No outfile; no style
                -- mdconvert in
                then do
                    let result = convert incontents
                    case result of
                        Just output -> writeFile (((splitOn "." infile) !! 0) ++ ".html") output
                        Nothing     -> putStrLn "Error"
                else if length args == 3 && args !! 1 == "-s" && head (args !! 2) /= '-'
                    -- No outfile; with style
                    -- mdconvert in -s style
                    then do
                        let sfile = args !! 2
                        shandle <- openFile sfile ReadMode
                        scontents <- hGetContents shandle
                        let result = sconvert incontents scontents
                        case result of
                            Just output -> writeFile (((splitOn "." infile) !! 0) ++ ".html") output
                            Nothing     -> putStrLn "Error"
                    else if length args == 4 && head (args !! 1) /= '-' 
                                && args !! 2 == "-s" && head (args !! 3) /= '-'
                        -- Outfile present; with style
                        -- mdconvert in out -s style
                        then do
                            let sfile = args !! 3
                            shandle <- openFile sfile ReadMode
                            scontents <- hGetContents shandle
                            let result = sconvert incontents scontents
                            case result of
                                Just output -> writeFile (args !! 1) output
                                Nothing     -> putStrLn "Error"
                        -- Outfile present; no style
                        -- mdconvert in out
                        else if length args == 2 && head (args !! 1) /= '-'
                            then do
                                let result = convert incontents
                                case result of
                                    Just output -> writeFile (args !! 1) output
                                    Nothing     -> putStrLn "Error"
                            else do printUsage

