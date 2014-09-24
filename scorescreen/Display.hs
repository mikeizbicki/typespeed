module Display where

{- to Do:
    [x] edit file to take parameter for Title of Page
    [ ] make a page for each group (Global, you, ta's, sections, time) --Jon
    [ ] beautify it --Dat
    [ ] add top 10 addicts and avg to each page + std dev --Dat
    [ ] create navigation pane --Jon
-}

import qualified Data.ByteString.Char8 as B

-- prints html to specified file
display :: String -> String -> [[B.ByteString]] -> IO ()
display title fileLoc bss = do
       writeFile fileLoc $ file title $ listToTable $ removeJunk bss
       
 
-- creates html string      
file :: String -> String -> String
file title x =    "<!DOCTYPE html>\n"
               ++ "<html>\n"
               ++ "<head>\n"
               ++ "<link type=\"text/css\" rel=\"stylesheet\" href=\"stylesheet.css\" >\n"
               ++ "<title>" ++ title ++ "</title>\n"
               ++ "</head>\n"
               ++ "<body>\n"
               ++ "<h1>" ++ title ++ "</h1>"
               ++ x 
               ++ "</body>\n"
               ++ "</html>\n"


-- converts scores into html table format
listToTable :: [[B.ByteString]] -> String
listToTable bss =    "<center>\n"
                  ++ "<table>\n"
                  ++ "<tr>\n"
                  ++ "<th>Score</th>\n<th>Username</th>\n<th>Chars Entered</th>\n<th>Words Entered</th>\n<th>Duration (s)</th>\n"
                  ++ "</tr>\n"
                  ++ rows bss
                  ++ "</table>\n"
                  ++ "</center>\n"
    where rows [] = ""
          rows (bs':bss') =    "<tr>\n"
                            ++ entries bs'
                            ++ "</tr>\n"
                            ++ rows bss'
          entries [] = ""
          entries (b'':bs'') =    "<td>"
                               ++ B.unpack b''
                               ++ "</td>\n"
                               ++ entries bs''

-- removes unwanted data from scores, leaving score, total count, enter offset,
-- name, and duration
removeJunk :: [[B.ByteString]] -> [[B.ByteString]]
removeJunk [] = []
removeJunk (bs:bss) = ((head bs) : (bs !! 3) : take 2 (tail bs) ++ [timeify (bs !! 6)]) : removeJunk bss
    where timeify b = B.pack $ show $ (read $ B.unpack b) / 100.0
