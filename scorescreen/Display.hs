module Display where

import           Data.List
import qualified Data.ByteString.Char8 as B
import           Math.Statistics
import           Calc
import           System.Directory
import           System.FilePath.Posix


-- Prints html to specified file
display :: String -> String -> String -> [[B.ByteString]] -> IO ()
display userID title fileLoc bss = do
       groupList' <- getDirectoryContents "groups"
       let groupList = sort $ filter (\x -> if head x == '.' then False else True) groupList' -- removes '.' and '..' directories
       names <- getNames groupList
       writeFile fileLoc $ (htmlPage title) ++ (file "Your Scores" (listToTable $ removeJunk $ top10 you) "" (show $ mean $ getScore you) (show $ stddev $ getScore you)) ++ (file "All Scores" (listToTable $ removeJunk $ top10 bss) (listToAddicts $ top10Addicts bss) (show $ mean $ getScore bss) (show $ stddev $ getScore bss)) ++ (displayGroups groupList names bss) ++ "<br><br><p>Web Display By: <a href=\"https://github.com/jduga002\">Jonathan Dugan</a> & <a href=\"https://github.com/cheripai/\">Dat Do</a><p>\n" ++ "</body>\n" ++ "</html>\n"
          where getNames [] = return []
                getNames (x:xs) = do
                         contents <- fmap lines $ readFile $ "groups/" ++ x
                         restOfContents <- getNames xs
                         return $ contents:restOfContents
                you = filterGroup[userID] bss

       
-- Generates header section for html page
htmlPage :: String -> String
htmlPage title =    "<!DOCTYPE html>\n"
                 ++ "<html>\n"
                 ++ "<head>\n"
                 ++ "<link type=\"text/css\" rel=\"stylesheet\" href=\"stylesheet.css\" >\n"
                 ++ "<title>" ++ title ++ "</title>\n"
                 ++ "</head>\n"
                 ++ "<body>\n"
                 ++ forkMe
                 ++ "<div id=\"header\">\n"
                 ++ "<div id=\"navbar\">\n"
                 ++ "<ul>\n"
                 ++ "<li><a href=\"alltime.html\">All Time</a></li>\n"          
                 ++ "<li><a href=\"quarter.html\">This Quarter</a></li>\n"     
                 ++ "<li><a href=\"month.html\">This Month</a></li>\n"
                 ++ "<li><a href=\"week.html\">This Week</a></li>\n"
                 ++ "</ul>\n"
                 ++ "</div>\n"
                 ++ "</div>\n"
                 ++ "<br>\n"
                 ++ "<br>\n"
                 ++ "<center><img src=\"./img/typespeedlogo.png\"/></center>\n"
                 ++ "<br>\n"
        where forkMe = "<a href=\"https://github.com/mikeizbicki/typespeed\"><img style=\"position: absolute; top: 0; right: 0; border: 0;\" src=\"https://camo.githubusercontent.com/52760788cde945287fbb584134c4cbc2bc36f904/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f77686974655f6666666666662e706e67\" alt=\"Fork me on GitHub\" data-canonical-src=\"https://s3.amazonaws.com/github/ribbons/forkme_right_white_ffffff.png\"></a>\n"


-- A very nasty function - Good Luck!
displayGroups :: [String] -> [[String]] -> [[B.ByteString]] -> String
displayGroups [] _ _ = ""
displayGroups _ [] _ = ""
displayGroups (x:xs)(g:gs) bss = file (takeFileName x) top10_gss top10Addicts_gss avg dev ++ displayGroups xs gs bss
    where gss = filterGroup g bss
          top10_gss = listToTable $ removeJunk $ top10 gss
          top10Addicts_gss = listToAddicts $ top10Addicts gss
          avg = show $ mean $ getScore gss
          dev = show $ stddev $ getScore gss
                  

-- Creates each section of page
file :: String -> String -> String -> String -> String -> String
file title top addicts avg dev =    "<hr />\n"
                                 ++ "<h2>" ++ title ++ "</h2>\n"
                                 ++ top 
                                 ++ addicts
                                 ++ "<br>\n"
                                 ++ "<br>\n"
                                 ++ "<strong>Average score: " ++ avg ++ "</strong>\n"
                                 ++ "<br>\n"
                                 ++ "<br>\n"
                                 ++ "<strong>Standard deviation: " ++ dev ++ "</strong>\n"
                                 ++ "<br><br>\n"


-- Converts scores into html table format for all stats
listToTable :: [[B.ByteString]] -> String
listToTable bss =    "<table id=\"tleft\">\n"
                  ++ "<tr>\n"
                  ++ "<tr id=\"title\"><td colspan=\"5\">&#8212 Top 10 Scores &#8212</td></tr>\n"
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


-- Converts scores into html table format for addicts
listToAddicts :: [[B.ByteString]] -> String
listToAddicts []  = ""
listToAddicts bss =  "<table id=\"tright\">\n"
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


-- Removes unwanted data from scores. Leaving score, total count, enter offset,
-- name, and duration
removeJunk :: [[B.ByteString]] -> [[B.ByteString]]
removeJunk [] = []
removeJunk (bs:bss) = ((head bs) : (bs !! 3) : take 2 (tail bs) ++ [timeify (bs !! 6)]) : removeJunk bss
    where timeify b = B.pack $ show $ (read $ B.unpack b) / 100.0
