module Main where 

import Data.Char (isDigit, isSpace)

data JsonValue = JsonNull 
               | JsonBool Bool
               | JsonNumber Integer
               | JsonString String 
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)


removeWhiteSpace :: String -> String 
removeWhiteSpace text = dropWhile isSpace text


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
jsonValueParser input =
    let text = removeWhiteSpace input in  
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
parseElems input = 
    case removeWhiteSpace input of 
        -- Base case
        (']':rest) -> Just (rest, [])
        -- Find which type of JsonValue by calling jsonValueParser
        text       -> case jsonValueParser text of 
                        Nothing -> Nothing 
                        Just (removeAfter, val) -> case removeWhiteSpace removeAfter of 
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
parsePairs input =
    case removeWhiteSpace input of 
        -- Base case
        ('}':rest) -> Just (rest, [])
        text       -> case jsonStringParser text of 
                        -- Find key, return "key"
                        Just (removeAfterKey, JsonString key) -> case removeWhiteSpace removeAfterKey of 
                                                                    -- Find colon, it can be anything behind it, thus call jsonValueParser
                                                                    (':':removeAfterColon) -> case jsonValueParser removeAfterColon of
                                                                                                -- 
                                                                                                Just (removeAfterValue, val) -> case removeWhiteSpace removeAfterValue of 
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
    case jsonValueParser (removeWhiteSpace input) of
        Just (remainder, val) -> if all isSpace remainder then Just val else Nothing 
        Nothing               -> Nothing

{-
TODO: 

Print function 
Lookup/deep search function to find any field
-}

-- Search on root level
findKey :: String -> JsonValue -> Maybe JsonValue 
findKey search (JsonObject pairs) = lookup search pairs 
findKey _ _ = Nothing  


-- Search nested JSON
-- Handles depth while helper function handles breadth
search :: String -> JsonValue -> Maybe JsonValue 
-- First try JsonObject
search target (JsonObject pairs) = 
    case lookup target pairs of 
        Just val -> Just val 
        Nothing  -> findInList target [v | (_, v) <- pairs]
-- Then try JsonArray 
search target (JsonArray elems) = 
    findInList target elems 
-- Remaning 4 datatypes are non recursive - cant go deeper 
search _ _  = Nothing 


findInList :: String -> [JsonValue] -> Maybe JsonValue 
findInList target [] = Nothing 
findInList target (x:xs) = 
    case search target x of 
        Just ok -> Just ok 
        Nothing -> findInList target xs 


printTerminal :: JsonValue -> String 
printTerminal jsonValue = printNice 0 jsonValue


-- d = number of spaces for indentation
printNice :: Int -> JsonValue -> String 
printNice d JsonNull           = "null"
printNice d (JsonBool b)       = if b then "true" else "false"
printNice d (JsonNumber n)     = show n 
printNice d (JsonString s)     = "\"" ++ s ++ "\""
printNice d (JsonArray xs)     = 
    "[\n" ++ concatWithComma (map (printNice (d + 2)) xs) (d + 2) ++ "\n" ++ indent d ++ "]"
printNice d (JsonObject pairs) = 
    "{\n" ++ concatWithComma (map (printPair (d + 2)) pairs) (d + 2) ++ "\n" ++ indent d ++ "}"
    where 
        printPair depth (k, v) = indent depth ++ "\"" ++ k ++ "\": " ++ printNice depth v


indent :: Int -> String 
indent n = replicate n ' '


concatWithComma :: [String] -> Int -> String 
concatWithComma [] _     = ""
concatWithComma [x] _    = x
concatWithComma (x:xs) d = x ++ ",\n" ++ indent d ++ concatWithComma xs d  

searchTerminal :: JsonValue -> IO ()
searchTerminal root = do 
    putStr "\n Enter a key to search for: "
    key <- getLine 

    if key == "q" then putStrLn "Closing." else do 
        case search key root of 
            Just found -> do 
                putStrLn (printTerminal found)
            Nothing    -> do 
                putStrLn "Key doesn't exist"
        searchTerminal root

main :: IO ()
main = do
    -- Read into a string
    -- data.json contains example from https://json.org/example.html 
    content <- readFile "data.json"
    case parseJson content of 
        Just root -> do 
            putStrLn "\n'data.json' loaded."
            putStrLn "-------------------\n"
            putStrLn (printTerminal root) 
            searchTerminal root
        Nothing   -> putStrLn "error loading file."