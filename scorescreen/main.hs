import qualified Data.ByteString.Char8 as B
import           Data.List

{-
    TODO:
    [x] Create 2d list
    [x] Top 10 scores
    [x] Average score
    [ ] Split list into groups (TA, sections, scores of the week, user progress, etc)
    [ ] Top 10 addicts
-}

-- Score Format:
-- score    total count    enter offset    name    word list    rule set    duration    time


main = do
    contents <- fmap B.lines $ B.readFile "/usr/local/var/games/typespeed.score" 
    let score = wordsLines contents -- Contains the 2d list of scores

    putStr "Dat's scores \n"
    let dat = filterGroup ["dat"] score
    print dat
    
    putStr "Jon's scores \n"
    let jon = filterGroup ["jon"] score
    print jon
	
    putStr "Top 10: \n"
    let top = top10 score
    print top

    putStr "\nAverage score: "
    print $ averageScore score


-- Calls words on all lines of a list
-- Used to create 2d array out of list of scores
wordsLines :: [B.ByteString] -> [[B.ByteString]]
wordsLines [] = []
wordsLines (x:xs) = (B.words x) : (wordsLines xs)


-- Gets top 10 scores
top10 :: [[B.ByteString]] -> [[B.ByteString]]
top10 x = take 10 $ reverse $ sort x


-- Returns average score from total score
averageScore :: [[B.ByteString]] -> Int
averageScore x = (totalScore x) `div` length x


-- Returns total score from 2d score list
totalScore :: [[B.ByteString]] -> Int 
totalScore [] = getScore []
totalScore [x] = getScore x
totalScore (x:xs) = getScore x + totalScore xs


-- Returns the score as an Int from single score entry
getScore :: [B.ByteString] -> Int
getScore [] = 0
getScore x = read (B.unpack (x !! 0)) :: Int

-- Generic function for filtering list of score entries by certain groups
filterGroup :: [String] -> [[B.ByteString]] -> [[B.ByteString]]
-- filterGroup gs [] = []
filterGroup gs bss = filter (elemGroup) bss
    where elemGroup bs = getUser bs `elem` gs

--returns the user as a B.Bytestring from single score entry
getUserBS :: [B.ByteString] -> B.ByteString
getUserBS [] = B.empty
getUserBS x = x !! 3

--returns the user as a String from single score entry
getUser :: [B.ByteString] -> String
getUser = B.unpack . getUserBS