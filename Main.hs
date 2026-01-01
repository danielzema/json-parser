module Main where 

data JsonValue = JsonNull 
               | JsonBool Bool

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

-- Parse JsonNull, return Nothing if text isn't "null"
jsonNullParser :: String -> Maybe (String, JsonValue)
jsonNullParser text = 
    case stringParser "null" text of 
        -- If it return Just it means that it found null
        Just (remainder, _) -> Just (remainder, JsonNull)
        Nothing             -> Nothing

jsonBoolParser :: String -> Maybe (String, JsonValue)
jsonBoolParser text = 
    case stringParser "true" text of 
        Just (remainder, _) -> Just (remainder, JsonBool True)
        Nothing             -> case stringParser "false" text of 
                                   Just (remainder, _) -> Just (remainder, JsonBool False)
                                   Nothing             -> Nothing 


main :: IO ()
main = undefined