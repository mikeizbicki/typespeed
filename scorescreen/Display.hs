module Display where

{- to Do:
    [x] edit file to take parameter for Title of Page
    [ ] make a page for each group (Global, you, ta's, sections, time) --Jon
    [ ] beautify it --Dat
    [x] add top 10 addicts and avg to each page + std dev --Dat
    [ ] create navigation pane --Jon
-}

import qualified Data.ByteString.Char8 as B

-- prints html to specified file
display :: String -> String -> [[B.ByteString]] -> [[B.ByteString]] -> Float -> Float -> IO ()
display title fileLoc bss addicts avg dev = do
       writeFile fileLoc $ file title (listToTable (removeJunk bss) title) (listToAddicts addicts) (show avg) (show dev)
       
 
-- creates html string      
file :: String -> String -> String -> String -> String -> String
file title top addicts avg dev =    "<!DOCTYPE html>\n"
                                 ++ "<html>\n"
                                 ++ "<head>\n"
                                 ++ "<link type=\"text/css\" rel=\"stylesheet\" href=\"stylesheet.css\" >\n"
                                 ++ "<title>" ++ title ++ "</title>\n"
                                 ++ "</head>\n"
                                 ++ "<body>\n"
                                 ++ "<div id=\"header\">\n"
                                 ++ "<div id=\"navbar\">\n"
                                 ++ "<ul>\n"
                                 ++ "<li><a href=\"https://www.google.com/\">All Scores</a></li>\n"          -- need to create function to fill in
                                 ++ "<li><a href=\"https://www.google.com/\">TA Scores</a></li>\n"           -- however many headings there are
                                 ++ "<li><a href=\"https://www.google.com/\">Section Scores</a></li>\n"
                                 ++ "<li><a href=\"https://www.google.com/\">Your Scores</a></li>\n"
                                 ++ "</ul>\n"
                                 ++ "</div>\n"
                                 ++ "</div>\n"
                                 ++ "<div>\n"
                                 ++ "<br>\n"
                                 ++ "<br>\n"
                                 ++ "<center><img src=\"./img/typespeedlogo.png\"/></center>"
                                 ++ "<br>\n"
                                 ++ "<br>\n"
                                 ++ top 
                                 ++ addicts
                                 ++ "<br>\n"
                                 ++ "<br>\n"
                                 ++ "<strong>Average score: " ++ avg ++ "</strong>\n"
                                 ++ "<br>\n"
                                 ++ "<br>\n"
                                 ++ "<strong>Standard deviation: " ++ dev ++ "</strong>\n"
                                 ++ "</div>\n"
                                 ++ "</body>\n"
                                 ++ "</html>\n"


-- converts scores into html table format for all stats
listToTable :: [[B.ByteString]] -> String -> String
listToTable bss title =    "<table id=\"tleft\">\n"
                  ++ "<tr>\n"
                  ++ "<tr id=\"title\"><td colspan=\"5\">&#8212 " ++ title ++ " &#8212</td></tr>\n"
                  ++ "<th>Score</th>\n<th>Username</th>\n<th>Chars Entered</th>\n<th>Words Entered</th>\n<th>Duration (s)</th>\n"
                  ++ "</tr>\n"
                  ++ rows bss
                  ++ "</table>\n"
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


-- converts scores into html table format for addicts
listToAddicts :: [[B.ByteString]] -> String
listToAddicts bss =  
                     "<table id=\"tright\">\n"
                  ++ "<tr>\n"
                  ++ "<tr id=\"title\"><td colspan=\"2\">&#8212 Top 10 Addicts &#8212</td></tr>\n"
                  ++ "<th>Times Played</th>\n<th>Username</th>\n"
                  ++ "</tr>\n"
                  ++ rows bss
                  ++ "</table>\n"
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
