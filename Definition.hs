module Definition (parseDescr, parseDescrFromFile, DescrLine, image, alttxt, txtfile, prev, next) where 

import Data.List
import Text.ParserCombinators.Parsec
import Debug.Trace

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
packDescr (i:p:n:a:t:_) = trace (show $ DescrLine (read i) (read p) (read n) (read t) a) DescrLine (read i) (read p) (read n) (read t) a
packDescr x = error $ "packDescr: wrong number of fields in" ++ (show x)
-- packDescr i:p:n:t:as = DescrLine i p n t $ concat as


{- Description File Parser using Parsec
source: http://book.realworldhaskell.org/read/using-parsec.html
TODO: make parser accept and ignore empty lines
TODO: accept missing \n on last line
-}

descrFile = sepEndBy line eol
line = sepBy cell (char ';') 
--emptyline = spaces
cell = many (noneOf ";\n")
eol = char '\n' 

parseDescr :: String -> Either ParseError [DescrLine]
parseDescr input = case parse descrFile "(unknown)" input of
  (Left e)  -> Left e
  (Right l) -> Right $ map packDescr $ filter (/= [""]) l


parseDescrFromFile:: String -> IO (Either ParseError [DescrLine])
parseDescrFromFile infile = parseFromFile descrFile infile
                            >>= (\e -> return $
                                       e >>= return.(map packDescr).(filter (/= [""])) ) 