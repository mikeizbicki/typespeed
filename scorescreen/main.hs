import qualified Data.ByteString.Char8 as B

-- Score Format:
-- score    total count    enter offset    name    word list    rule set    duration    time


main = do
    contents <- fmap B.lines $ B.readFile "/usr/local/var/games/typespeed.score" 
    let score = wordsLines contents
    print $ averageScore score


-- Calls words on all lines of a list
-- Used to create 2d array out of list of scores
wordsLines :: [B.ByteString] -> [[B.ByteString]]
wordsLines [] = []
wordsLines (x:xs) = [B.words x] ++ wordsLines xs


-- Returns total score from 2d score list
totalScore :: [[B.ByteString]] -> Int 
totalScore [] = getScore []
totalScore [x] = getScore x
totalScore (x:xs) = getScore x + totalScore xs


-- Returns average score from total score
averageScore :: [[B.ByteString]] -> Int
averageScore x = (totalScore x) `div` length x


-- Returns the score as an Int from single score entry
getScore :: [B.ByteString] -> Int
getScore [] = error "getScore: Input does not exist"
getScore x = read (B.unpack (x !! 0)) :: Int
