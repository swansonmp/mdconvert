-- Markdown to HTML Converter
-- Matthew Swanson

import Data.List.Split (splitOn)
import Data.Char (isSpace)

type Text = String
type Href = String
type Src  = String
type Type = String

data Tag = H1 String
         | H2 String
         | H3 String
         | H4 String
         | H5 String
         | H6 String
         | P [Tag]
         | Em Text
         | Strong Text
         | Code Text
         | HR
         | Img Src
         | Video Src Type
         | Audio Src Type
         | Blockquote [Tag]
         | A Href Text
         | UL [Tag]
         | OL [Tag]
         | LI [Tag]
         | Text String
         | Out String
         | Err
            deriving Show

sr :: ([Tag], [Tag]) -> ([Tag], [Tag])
sr s = ([], [])

readInline :: String -> String
readInline s = replace '<' "&lt;" (replace '>' "&gt;" s)

replace :: Char -> String -> String -> String
replace c r (h:s)
    | c == h    = r ++ (replace c r s)
    | otherwise = h : (replace c r s)
replace c r [] = ""

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

-- Converts a single string into a token, producing error on non-tokens
readBlock :: String -> Tag
readBlock ('#':'#':'#':'#':'#':'#':cs) = H6 cs
readBlock ('#':'#':'#':'#':'#':cs) = H5 cs
readBlock ('#':'#':'#':'#':cs) = H4 cs
readBlock ('#':'#':'#':cs) = H3 cs
readBlock ('#':'#':cs) = H2 cs
readBlock ('#':cs)  = H1 cs
readBlock ('-':'-':'-':cs) = HR
readBlock ('!':'(':cs) = parseImg ('!':'(':cs)
readBlock ('+':'(':cs) = parseVideo ('+':'(':cs)
readBlock ('@':'(':cs) = parseAudio ('@':'(':cs)
readBlock ('<':' ':cs) = Blockquote [P [Text (readInline (init (init cs)))]]
readBlock s         = P [Text (readInline s)]


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

toString :: [Tag] -> String
toString ((H1 s):ts) = "<h1>" ++ s ++ "</h1>\n" ++ (toString ts)
toString ((H2 s):ts) = "<h2>" ++ s ++ "</h2>\n" ++ (toString ts)
toString ((H3 s):ts) = "<h3>" ++ s ++ "</h3>\n" ++ (toString ts)
toString ((H4 s):ts) = "<h4>" ++ s ++ "</h4>\n" ++ (toString ts)
toString ((H5 s):ts) = "<h5>" ++ s ++ "</h5>\n" ++ (toString ts)
toString ((H6 s):ts) = "<h6>" ++ s ++ "</h6>\n" ++ (toString ts)
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
toString ((P t):ts) = "<p>" ++ (toString t) ++ "</p>\n" ++ (toString ts)
toString ((Text t):ts) = t
toString (_:ts) = "" ++ (toString ts)
toString [] = ""
                    
wrap :: String -> String
wrap s = "<html>\n" ++ s ++ "</html>\n"

convert :: String -> Maybe String
--convert s = parser (lexer s)
convert s = Just (wrap (toString (lexer s)))

main = do
    contents <- getContents
    let result = convert contents
    --let result = Just (concat (preproc contents))
    case result of
        Just output ->
            writeFile "output.html" (output)
        Nothing -> putStrLn "Error"


