import qualified Data.ByteString.Char8 as B
import           System.Posix.User
import           Data.Time.Clock.POSIX
import           Calc
import           Display
import           System.Directory
import           System.Cmd



-- Score Format:
-- score    total count    enter offset    name    word list    rule set    duration    time


main = do
    contents <- fmap B.lines $ B.readFile "/usr/local/var/games/typespeed.score" 
    let score = wordsLines contents -- Contains the 2d list of scores

    userID <- getLoginName

    curTime <- fmap round getPOSIXTime -- gets the current time as an integer
    let week = 604800 -- length of a week in seconds
    let month = 2627942 -- length of average month in seconds
    let fall = 1412208000
    let winter = 1420416000
    let spring = 1427673600
    let quarter = curTime - fall -- Change me for next quarter!
    let weekScore = filterTime week curTime score
    let monthScore = filterTime month curTime score
    let quarterScore = filterTime quarter curTime score
 

    home <- getHomeDirectory

    -- Generates the HTML pages
    display userID "All Time" (home ++ "/.typespeed/alltime.html") score
    display userID "This Quarter" (home ++ "/.typespeed/quarter.html") quarterScore
    display userID "This Month" (home ++ "/.typespeed/month.html") monthScore
    display userID "This Week" (home ++ "/.typespeed/week.html") weekScore


    -- Opens generated page in firefox
    system $ "firefox " ++ home ++ "/.typespeed/alltime.html"
