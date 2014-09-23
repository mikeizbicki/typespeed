module Display where

import qualified Data.ByteString.Char8 as B

display :: String -> IO ()
display x = do
       writeFile "index.html" $ file x
       
       
file :: String -> String
file x =    "<!DOCTYPE html>\n"
         ++ "<html>\n"
         ++ "<head>\n"
         ++ "<title>Score Page</title>\n"
         ++ "</head>\n"
         ++ "<body>\n"
         ++ x 
         ++ "</body>\n"
         ++ "</html>\n"
