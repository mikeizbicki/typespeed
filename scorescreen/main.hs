import qualified Data.ByteString.Char8 as B

-- Score Format:
-- score    total count    enter offset    name    word list    rule set    duration    time


main = do
    contents <- fmap B.lines $ B.readFile "/usr/local/var/games/typespeed.score" 
    let score = wordsLines contents
    print score


-- Calls words on all lines of a list
-- Used to create 2d array out of list of scores
wordsLines :: [B.ByteString] -> [[B.ByteString]]
wordsLines [] = []
wordsLines (x:xs) = [B.words x] ++ wordsLines xs


-- Returns average score as a bytestring
averageScore :: [[B.ByteString]] -> Int  
averageScore [[]] = 0
