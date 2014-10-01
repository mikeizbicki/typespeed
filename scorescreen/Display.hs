module Display where

{-
    Fall 2014:   1412208000
    Winter 2015: 1420416000
    Spring 2015: 1427673600
-}

import           Data.List
import qualified Data.ByteString.Char8 as B
import           Math.Statistics
import           Calc
import           System.Directory
import           System.FilePath.Posix

-- prints html to specified file
display :: String -> String -> [[B.ByteString]] -> IO ()
display title fileLoc bss = do
       groupList' <- getDirectoryContents "groups"
       let groupList = sort $ filter (\x -> if head x == '.' then False else True) groupList'
       names <- getNames groupList
       writeFile fileLoc $ (htmlPage title) ++ (file (listToTable (removeJunk bss) "") (listToAddicts (top10Addicts bss) "") (show $ mean $ getScore bss) (show $ stddev $ getScore bss)) ++ (displayGroups groupList names bss) ++ "</body>\n" ++ "</html>\n"
          where getNames [] = return []
                getNames (x:xs) = do
                         contents <- fmap lines $ readFile $ "groups/" ++ x
                         restOfContents <- getNames xs
                         return $ contents:restOfContents

       
htmlPage :: String -> String
htmlPage title =    "<!DOCTYPE html>\n"
                 ++ "<html>\n"
                 ++ "<head>\n"
                 ++ "<link type=\"text/css\" rel=\"stylesheet\" href=\"stylesheet.css\" >\n"
                 ++ "<title>" ++ title ++ "</title>\n"
                 ++ "</head>\n"
                 ++ "<body>\n"
                 ++ "<div id=\"header\">\n"
                 ++ "<div id=\"navbar\">\n"
                 ++ "<ul>\n"
                 ++ "<li><a href=\"alltime.html\">All Time</a></li>\n"          -- need to create function to fill in
                 ++ "<li><a href=\"https://www.google.com/\">This Quarter</a></li>\n"           -- however many headings there are
                 ++ "<li><a href=\"https://www.google.com/\">This Month</a></li>\n"
                 ++ "<li><a href=\"https://www.google.com/\">This Week</a></li>\n"
                 ++ "</ul>\n"
                 ++ "</div>\n"
                 ++ "</div>\n"
                 ++ "<br>\n"
                 ++ "<br>\n"
                 ++ "<center><img src=\"./img/typespeedlogo.png\"/></center>\n"

-- A very nasty function - Good Luck!
displayGroups :: [String] -> [[String]] -> [[B.ByteString]] -> String
displayGroups [] _ _ = ""
displayGroups _ [] _ = ""
displayGroups (x:xs)(g:gs) bss = file (listToTable (removeJunk gss ) $ takeFileName x) (listToAddicts (top10Addicts gss) $ takeFileName x) (show $ mean $ getScore gss) (show $ stddev $ getScore gss) ++ displayGroups xs gs bss
    where gss = filterGroup g bss
                  

-- creates html string      
file :: String -> String -> String -> String -> String
file top addicts avg dev =       "<br><br>\n"
                                 ++ top 
                                 ++ addicts
                                 ++ "<br>\n"
                                 ++ "<br>\n"
                                 ++ "<strong>Average score: " ++ avg ++ "</strong>\n"
                                 ++ "<br>\n"
                                 ++ "<br>\n"
                                 ++ "<strong>Standard deviation: " ++ dev ++ "</strong>\n"
                                 


-- converts scores into html table format for all stats
listToTable :: [[B.ByteString]] -> String -> String
listToTable bss title =    "<table id=\"tleft\">\n"
                  ++ "<tr>\n"
                  ++ "<tr id=\"title\"><td colspan=\"5\">&#8212 Top " ++ title ++ " Scores &#8212</td></tr>\n"
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
listToAddicts :: [[B.ByteString]] -> String -> String
listToAddicts [] _ = ""
listToAddicts bss title =  
                     "<table id=\"tright\">\n"
                  ++ "<tr>\n"
                  ++ "<tr id=\"title\"><td colspan=\"2\">&#8212 Top " ++ title ++ " Addicts &#8212</td></tr>\n"
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
