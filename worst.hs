import System.Environment
import System.FilePath
import System.Directory
import qualified Data.List

import qualified Data.Text.Lazy as T

import  Text.ParserCombinators.Parsec
import Text.Printf

import Debug.Trace

import Definition
import FileName

{- TODO
replaceStrByStr
replaceStrByFile

generateDir
checklinks

{- Apply generation to selected subdirectories
   Bonus: if template|description are newer than generated files
-}
recurseSubdirs
-}



{- generate prev and next links of a list of files
 each tuple contains (current Filename, prev, next)
-}
pageseqlinker:: [(FileName, FileName, FileName)] -> [(FileName, FileName, FileName)]
pageseqlinker xs = let (f,p,n)=unzip3 $ trace (show xs)  xs
                       genPrevs f p = replaceWithPrevL f p
                       genNexts f n = reverse $ replaceWithPrevL (reverse f) (reverse n)
                   in zip3 f  (genPrevs f p) (genNexts f n)
{-
                   reverse
                   $ foldl (\a (f,p,n) -> (f, repl p (first3$head a), n):a) []
 		   $ reverse	 
	           $ foldl (\a (f,p,n) -> (f,p,repl n (first3$head a)):a) []
                   $ trace (show xs) unzip3 xs
  -}
	where
		first3 (a,_,_) = a
		repl (FileName p) _ = FileName p
		repl FileNone _     = FileNone -- TODO: handle no input
		repl _ l            = l
                {- apply repl to r -}
                replaceWithPrevL l (r:rs)  = r:(map (\(l,r) -> repl l r) $ zip (init l) rs)


{- replace arrows <- and -> by actual file names-}
linkDescriptions:: [(FileName,DescrLine)] -> [(FileName,DescrLine)]
linkDescriptions l =   map (\((f,d),(_,p,n)) -> (f,d {prev=p, next=n}))
                       $ (zip l)
                       $ reverse $ pageseqlinker $ reverse -- FIXME: why is the list in wrong order
                       $ map (\(f,d) -> (f, prev d, next d)) l



{- Generate names of final files -}
genTargetFileNames:: String -> [DescrLine] -> [(FileName, DescrLine)]
genTargetFileNames template = snd.(foldl (\(i,l) d -> (i+1, (FileName $ trace (target i) target i, d):l) ) ((1::Int),[])) 
  where target i = concat [ dropTemplateSuffix $ dropExtensions $ takeFileName template
                          , seqNo i
                          , ".html"]
        dropTemplateSuffix:: String -> String
        dropTemplateSuffix t = T.unpack $ T.replace (T.pack "template") (T.pack "") $ T.pack t
        seqNo i | i < 10    = '0':(show i)
                | otherwise = show i
        






replaceMap:: [ ( String, (DescrLine -> String) ) ]
replaceMap = [ ("IMAGE", getFileName.image)
             , ("NEXT", getFileName.next)
             , ("PREV", getFileName.prev)
             , ("ALTTEXT" , alttxt)]

replaceFileMap:: [ ( String, (DescrLine -> IO String) ) ] 
replaceFileMap = [ ("PROJTEXT", (\d -> getCurrentDirectory
                                       >>= (\dir -> readFile$ getFileName$txtfile d)
                                )
                   )]



{- Replace from File -}
genFileFromDescription:: String -> DescrLine -> IO String
genFileFromDescription template d = ( execReplaceFileMap
                                      $ execReplaceStringMap (T.pack template))
                                    >>= return.T.unpack
  where
    execReplaceStringMap:: T.Text -> T.Text
    execReplaceStringMap = flip (foldl (\t (tag,key)
                                    -> T.replace (T.pack tag) (T.pack (key d)) t))
                       replaceMap

    execReplaceFileMap:: T.Text -> IO T.Text
    execReplaceFileMap text  = foldl (\t (tag,iokey) ->
                                       do key <- iokey d
                                          text <- t
                                          return $ T.replace (T.pack tag) (T.pack key) text)
                               (return text) replaceFileMap




main:: IO ()
main = do args <- getArgs -- template description
          template <- readFile (args !! 0)          
          description <-  parseDescrFromFile (args !! 1)
                          >>= (\either -> return
                                          $ either 
                                          >>= return.linkDescriptions.(genTargetFileNames $ args !! 0))
          case trace (show description) description of
            (Left e)   ->  print e
            (Right ds) -> printFiles template ds

{-
              foldl  (\_ (f,d) -> do text <-  genFileFromDescription template d
                                               writeFile (trace (getFileName f) $ getFileName f) text
                                  )
                           (return ()) ds
-}
       where
         printFiles _ []         = return ()
         printFiles template ((f,d):ds) = genFileFromDescription template d
                                          >>= writeFile (getFileName f) 
                                              >>= (\_ -> printFiles template ds )
                           
        