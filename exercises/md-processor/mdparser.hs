module MDProcessor where
import Data.Char(toLower)

data Lang = Haskell | C | Python | None deriving (Eq, Show)
data HeadingSize = H1 | H2 | H3 | H4 deriving (Eq, Show)
data ListMode = Ordered | Unordered deriving (Eq, Show)

data Element =
        Text String
      | Bold Element
      | Italic Element
      | Underline Element
      | Heading HeadingSize String
      | CodeBlock Lang [String]
      | List ListMode [Element]
      | HorizontalRule
      deriving (Eq, Show)

-- myList :: Element
-- myList = Ordered

myText :: Element
myText = Bold(Text "This is a test")
-- myText = List Unordered [Text "Elemen1", Text "Element2", Text "Element 3"]

headingPrefix :: HeadingSize -> String
headingPrefix H1 = "#"
headingPrefix H2 = "##"
headingPrefix H3 = "###"
headingPrefix H4 = "####"

htmlHeadingPrefix :: HeadingSize -> String
htmlHeadingPrefix H1 = "<h1>"
htmlHeadingPrefix H2 = "<h2>"
htmlHeadingPrefix H3 = "<h3>"
htmlHeadingPrefix H4 = "<h4>"

htmlHeadingPostfix :: HeadingSize -> String
htmlHeadingPostfix H1 = "</h1>"
htmlHeadingPostfix H2 = "</h2>"
htmlHeadingPostfix H3 = "</h3>"
htmlHeadingPostfix H4 = "</h4>"

lang :: Lang -> String
lang Haskell = "hs"
lang C = "c"
lang Python = "py"
lang None = ""

render2md :: Element -> String
render2md (Text s) = s
render2md (Bold el) = "**" ++ render2md el ++ "**"
render2md (Italic el) = "_" ++ render2md el ++ "_"
render2md (Underline el) = "__" ++ render2md el ++ "__"
render2md (Heading size str) = headingPrefix size ++ str
render2md (CodeBlock pl code) = "```" ++ lang pl ++ "\n" ++ unlines code ++ "```"
render2md HorizontalRule = "---"
render2md (List mode items) = renderList mode items
  where
    --renderList :: ListMode -> [Element] -> String
    renderList Ordered l = unlines [show i ++ ". " ++ render2md (l !! (i-1)) | i <- [1..length l]]
    renderList Unordered l = unlines ["- " ++ render2md x | x <- l]

render2html :: Element -> String
render2html (Text s) = "<p>" ++ s ++ "</p>"
render2html (Bold el) = "<b>" ++ render2html el ++ "</b>"
render2html (Italic el) = "<i>" ++ render2html el ++ "</i>"
render2html (Underline el) = "<u>" ++ render2html el ++ "</u>"
-- render2html (Heading size str) = htmlHeadingPrefix ++ render2html str ++ htmlHeadingPostfix
render2html (CodeBlock _ code) = "<code>" ++ unlines code ++ "</code>"
render2html (List mode items) | mode == Ordered = "<ol>" ++ renderList Ordered items ++ "</ol>"
                              | mode == Unordered = "<ul>" ++ renderList Unordered items ++ "</ul>"
    where
        renderList Ordered l = unlines ["<li>" ++ render2html (l !! (i - 1)) ++"</li>" | i <- [1..length l]]
        renderList Unordered l = unlines ["<li>" ++ render2html (l !! (i - 1)) ++"</li>" | i <- [1..length l]]

robberify :: Element -> Element
robberify (Text s) = Text (encode' s)
robberify (Bold el) = Bold (robberify el)
robberify (Italic el) = Italic (robberify el)
robberify (Underline el) = Underline (robberify el)
-- robberify (Heading size el) = Heading size (robberify el) I will work on those later :)
-- robberify (CodeBlock pl el) = CodeBlock pl (encode' el)
-- robberify (List el) = List (robberify el)
-- robberify (x el) = x robberify el -- Ask why this doesn't work!?!

-- toc :: [Element] -> [Element]
-- -- toc [] = List Ordered []
-- toc ((Heading H4 x) : xs) = Text (x) : toc(xs)
-- toc (x : xs) = toc(xs)

isChar :: Char -> Bool
isChar ch
    |toLower ch >= 'a' && toLower ch <= 'z' = True
isChar _ = False

isVowel :: Char -> Bool
isVowel ch
    | toLower ch == 'a' = True
    | toLower ch == 'e' = True
    | toLower ch == 'u' = True
    | toLower ch == 'i' = True
    | toLower ch == 'o' = True
    | toLower ch == 'y' = True
isVowel _ = False


isConsonant :: Char -> Bool
isConsonant ch
    | not (isChar ch) = False
    | isVowel ch = False
isConsonant _ = True

-- Bonus #1
encode' :: String -> String
encode' [] = []
encode' (x:xs)
    | isConsonant x = x : 'o' : toLower x : (encode' xs)
encode' (x:xs) = x : (encode' xs)
