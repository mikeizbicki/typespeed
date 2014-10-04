import qualified Data.ByteString.Char8 as B
import           Math.Statistics
import           System.Posix.User
import           Data.Time.Clock.POSIX
import           Calc
import           Display



-- Score Format:
-- score    total count    enter offset    name    word list    rule set    duration    time


main = do
    contents <- fmap B.lines $ B.readFile "/usr/local/var/games/typespeed.score" 
    let score = wordsLines contents -- Contains the 2d list of scores

    userID <- getLoginName
    putStr userID
    putStr " scores \n"
    let you = filterGroup[userID] score
    print you

    putStr "\nDat's scores \n"
    let dat = filterGroup ["dat"] score
    print dat
    
    putStr "\nJon's scores \n"
    let jon = filterGroup ["jon"] score
    print jon
	
    putStr "\nTop 10: \n"
    let top = top10 score
    print top

    putStr "\nAverage score: "
    print $ mean $ getScore score

    putStr "\nStandard deviation: "
    print $ stddev $ getScore score
    
    putStr "\nTop 10 addicts: \n"
    let addicts = top10Addicts score
    print addicts

    let week = 604800 -- length of a week in seconds
    curTime <- fmap round getPOSIXTime -- gets the current time as an integer
    let weekScore = filterTime week curTime score
    putStr "\nScores of the week: \n"
    print weekScore

 
    display userID "All Time" "alltime.html" score -- Creates HTML page displaying top as a string

