module Main where 

charParser :: Char -> String -> Maybe (String, Char)
charParser goal text =  
    case text of 
        x:xs | x == goal -> Just (xs, goal)
        _             -> Nothing

main :: IO ()
main = undefined