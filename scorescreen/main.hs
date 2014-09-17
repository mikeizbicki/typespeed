import qualified Data.ByteString.Char8 as B
import           Data.List

-- Score Format:
-- score    total count    enter offset    name    word list    rule set    duration    time


main = do
    contents <- fmap B.lines $ B.readFile "/usr/local/var/games/typespeed.score" 
    let score = wordsLines contents -- Contains the 2d list of scores

    putStr "Top 10: \n"
    let top = top10 score
    print top

    putStr "\nAverage score: "
    print $ averageScore score


-- Calls words on all lines of a list
-- Used to create 2d array out of list of scores
wordsLines :: [B.ByteString] -> [[B.ByteString]]
wordsLines [] = []
wordsLines (x:xs) = [B.words x] ++ wordsLines xs


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
