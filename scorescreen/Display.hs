module Display where

display :: IO ()
display = do
       writeFile "index.html" file
       
       
file :: String
file =    "<!DOCTYPE html>\n"
       ++ "<html>\n"
       ++ "<head>\n"
       ++ "<title>Score Page</title>\n"
       ++ "</head>\n"
       ++ "<body>\n"
       ++ "Stuff\n"
       ++ "</body>\n"
       ++ "</html>\n"
