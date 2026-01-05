# JSON Parser
Built in Haskell, from scratch, without any external libraries.

---

## Functionality
Reads a file data.json (which can contain any JSON) and converts it into a string, which is then parsed with support for the following datatypes:
``` Haskell
data JsonValue = JsonNull 
               | JsonBool Bool 
               | JsonNumber Integer 
               | JsonString String 
               | JsonArray [JsonValue] 
               | JsonObject [(String, JsonValue)]
```
A search function is not yet implemented.
