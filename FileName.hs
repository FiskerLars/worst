module FileName where
import Data.List
import Data.Char
import Text.ParserCombinators.Parsec


data FileName = FileName String | FilePrev | FileNext | FileNone

instance Show FileName where
  showsPrec _ (FileName s) =  shows s 
  showsPrec _ (FilePrev)   =  shows "<-" 
  showsPrec _ (FileNext)   =  shows "->"
  showsPrec _ (FileNone)   =  shows "" 


getFileName:: FileName -> String
getFileName (FileName s) = s
getFileName _ = error "FilePrev/FileNext/FileNone cannot be displayed as String"

instance Read FileName where
  readsPrec _ s =  [(readFilename $ strip s, "")]
    where
      readFilename s | s == "<-" = FilePrev
                     | s == "->" = FileNext
                     | s == ""   = FileNone
                     | otherwise = FileName s
      strip = dropWhile isSpace . dropWhileEnd isSpace