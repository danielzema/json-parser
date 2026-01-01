module Main where 

-- Parse a single char
charParser :: Char -> String -> Maybe (String, Char)
charParser goal text =  
    case text of 
        x:xs | x == goal -> Just (xs, goal)
        _             -> Nothing

-- Check if a word is in the beginning of a string
stringParser :: String -> String -> Maybe (String, String)
stringParser [] text = Just (text, [])
stringParser (firstGoal:restGoal) text = 
    case text of 
        (firstInput:restInput) | firstInput == firstGoal -> case stringParser restGoal restInput of 
                                Nothing -> Nothing 
                                Just (remainder, restResult) -> Just (remainder, firstGoal : restResult)
        _ -> Nothing



main :: IO ()
main = undefined