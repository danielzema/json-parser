module Main where 

import Data.Char (isDigit)

data JsonValue = JsonNull 
               | JsonBool Bool
               | JsonNumber Integer
               | JsonString String 
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

-- Parse a single char
charParser :: Char -> String -> Maybe (String, Char)
charParser goal text =  
    case text of 
        x:xs | x == goal -> Just (xs, goal)
        _                -> Nothing

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
-- null    
jsonNullParser :: String -> Maybe (String, JsonValue)
jsonNullParser text = 
    case stringParser "null" text of 
        -- If it return Just it means that it found null
        Just (remainder, _) -> Just (remainder, JsonNull)
        Nothing             -> Nothing

-- Parse booleans
-- true
jsonBoolParser :: String -> Maybe (String, JsonValue)
jsonBoolParser text = 
    case stringParser "true" text of 
        Just (remainder, _) -> Just (remainder, JsonBool True)
        Nothing             -> case stringParser "false" text of 
                                   Just (remainder, _) -> Just (remainder, JsonBool False)
                                   Nothing             -> Nothing 

-- Parse Integers, floats not supported
-- 123
jsonNumberParser :: String -> Maybe (String, JsonValue)
jsonNumberParser text = 
    let (digits, remainder) = span isDigit text in 
        case digits of 
            "" -> Nothing 
            _  -> Just (remainder, JsonNumber (read digits))

-- Parse a string to JsonString
-- "text"
jsonStringParser :: String -> Maybe (String, JsonValue)
jsonStringParser text = 
    case text of
        -- Opening quote 
        ('"':rest) -> 
            -- String body 
            let (stringjson, remainder) = span (/= '"') rest in 
                -- Closing quote
                case remainder of 
                    ('"':afterEndQuoation) -> Just (afterEndQuoation, JsonString stringjson)
                    _ -> Nothing 
        _ -> Nothing 

-- Test all different parsers
jsonValueParser :: String -> Maybe (String, JsonValue)
jsonValueParser text = 
    case jsonNullParser text of 
        Just x  -> Just x 
        Nothing -> case jsonBoolParser text of 
            Just x  -> Just x 
            Nothing -> case jsonNumberParser text of 
                Just x  -> Just x 
                Nothing -> case jsonStringParser text of 
                    Just x  -> Just x 
                    Nothing -> case jsonArrayParser text of 
                        Just x  -> Just x 
                        Nothing -> jsonObjectParser text

-- Parse JsonArray
jsonArrayParser :: String -> Maybe (String, JsonValue)
jsonArrayParser text = 
    case text of 
        -- After opening bracket, call helper function to parse all elems inside
        ('[':rest) -> case parseElems rest of 
            Just (afterEndBracket, elems) -> Just (afterEndBracket, JsonArray elems)
            Nothing                       -> Nothing 
        _          -> Nothing

parseElems :: String -> Maybe (String, [JsonValue])
parseElems text = 
    case text of 
        -- Base case
        (']':rest) -> Just (rest, [])
        -- Find which type of JsonValue by calling jsonValueParser
        _          -> case jsonValueParser text of 
                        Nothing -> Nothing 
                        Just (removeAfter, val) -> case removeAfter of 
                            -- Continue recursion if comma, more JsonValues to come
                            (',':removeAfterComma) -> case parseElems removeAfterComma of 
                                Nothing -> Nothing 
                                Just (removeFinal, restOfVals) -> Just (removeFinal, val : restOfVals)
                            -- End recursion if closing bracket
                            (']':removeFinal) -> Just (removeFinal, [val])
                            _                 -> Nothing

-- Parse JsonObject
jsonObjectParser :: String -> Maybe (String, JsonValue)
jsonObjectParser text = 
    case text of 
        ('{':rest) -> case parsePairs rest of 
            Just (afterBrace, pairs) -> Just (afterBrace, JsonObject pairs)
            Nothing                  -> Nothing
        _         -> Nothing

parsePairs :: String -> Maybe (String, [(String, JsonValue)])
parsePairs text =
    case text of 
        -- Base case
        ('}':rest) -> Just (rest, [])
        _          -> case jsonStringParser text of 
                        -- Find key, return "key"
                        Just (removeAfterKey, JsonString key) -> case removeAfterKey of 
                                                                    -- Find colon, it can be anything behind it, thus call jsonValueParser
                                                                    (':':removeAfterColon) -> case jsonValueParser removeAfterColon of
                                                                                                -- 
                                                                                                Just (removeAfterValue, val) -> case removeAfterValue of 
                                                                                                                                  -- Recursion until no more commas found
                                                                                                                                  (',':removeAfterComma) -> case parsePairs removeAfterComma of
                                                                                                                                                                Just (removeFinal, restPairs) -> Just (removeFinal, (key, val) : restPairs)
                                                                                                                                                                Nothing                       -> Nothing

                                                                                                                                  ('}':removeFinal)      -> Just (removeFinal, [(key, val)])
                                                                                                                                  _                      -> Nothing 
                                                                                                Nothing                      -> Nothing 
                                                                    _                      -> Nothing 
                        _                                     -> Nothing 
        
parseJson :: String -> Maybe JsonValue 
parseJson input = 
    case jsonValueParser input of 
        Just ("", val) -> Just val -- Parsed everything 
        Just (_, val)  -> Just val -- Stuff remains 
        Nothing        -> Nothing  -- Failed to parse

main :: IO ()
main = do
    -- Read into a string 
    text <- readFile "data.json"
    case parseJson text of 
        Just val -> print val 
        Nothing  -> putStrLn "Invalid JSON"