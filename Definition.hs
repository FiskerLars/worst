module Definition (parseDescr, parseDescrFromFile, DescrLine, image, alttxt, txtfile, prev, next) where 

import Data.List
import Text.ParserCombinators.Parsec

import FileName

{-| Parse a Description File, ;-separated lines that describe
<Imagepath>; <prev path>; <next path>; alttext ; <textfile path>
-}

data DescrLine = DescrLine {
  image   :: FileName,
  prev    :: FileName,
  next    :: FileName,
  txtfile :: FileName,
  alttxt  :: String } deriving Show

packDescr:: [String] -> DescrLine
packDescr (i:p:n:a:t:_) = DescrLine (read i) (read p) (read n) (read t) a
-- packDescr i:p:n:t:as = DescrLine i p n t $ concat as


{- Description File Parser using Parsec
source: http://book.realworldhaskell.org/read/using-parsec.html
-}

descrFile = endBy line eol
line = sepBy cell (char ';')
cell = many (noneOf ";\n")
eol = char '\n'

parseDescr :: String -> Either ParseError [DescrLine]
parseDescr input = case parse descrFile "(unknown)" input of
  (Left e)  -> Left e
  (Right l) -> Right $ map packDescr l


parseDescrFromFile:: String -> IO (Either ParseError [DescrLine])
parseDescrFromFile infile = parseFromFile descrFile infile
                            >>= (\e -> return $
                                       e >>= return.(map packDescr)) 