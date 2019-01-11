## Monadic Parsers for Imperative Programmers
### Avi Press

---

### What is parsing?

Loosely:

String -> YourDataType

---

### What is parsing?

```javascript
> new Date(Date.parse("Feb 24, 2019"))
Sun Feb 24 2019 00:00:00 GMT-0800 (Pacific Standard Time)
```

---

### What we're used to

For simple things, just checking a few conditions directly works fine. For instance, grabbing content between brackets:

```python
if s.startswith("[") and s.endswith("]"):
   return s.replace("[", "").replace("]", "")
``` 

---

### What we're used to

For simple things, just checking a few conditions directly works fine. For instance, grabbing content between brackets:

```python
if s.startswith("[") and s.endswith("]"):
   return s.replace("[", "").replace("]", "")
``` 

What about spaces?

```[ we_want_to_parse_this_too      ]```

---

### What we're used to

What about spaces?

```[ we_want_to_parse_this_too      ]```


It's often compelling to reach for a regex

```
\[\s*(\S+)\s*\]
```

This is fine, but quickly gets complicated and very challenging to read:

```
# a regex for parsing dates, good luck editing this after a year passes!
^(?:(?:(?:0?[13578]|1[02])(\/|-|\.)31)\1|(?:(?:0?[13-9]|1[0-2])(\/|-|\.)(?:29|30)\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:0?2(\/|-|\.)29\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:(?:0?[1-9])|(?:1[0-2]))(\/|-|\.)(?:0?[1-9]|1\d|2[0-8])\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$
```

---

### What we're used to

- Plain old property checking of strings
  - Pros:
    - Parse however you want
    - As powerful as you care to make it
  - Cons:
    - Ad-hoc property checking functions are fine until you need to generalize
    - Often ends up being much more code than a simple regex
    - Eventually end up reinventing the wheel

---

### What we're used to

- Regular expressions
  - Pros:
    - Declarative
    - Quite powerful
  - Cons:
    - Empirically a nightmare
    - Difficult to read, harder to debug
    - Error handling quickly becomes complex, difficult or impossible
    
---
    
### Haskell's solution: parser combinators

Tiny little parsing functions that you can use to build up your own parsers

For this talk, we'll be using the MegaParsec library.

---

### Some terminology

- Stream: The sequence of information we're trying to parse
- Token: A single element of the stream
- Lexeme: A logical collection of tokens, like a word and the following spaces

---

### Some terminology

```
[ stuff_to_parse    ]

^^^^^^ Tokens  ^^^^^^

^-^--- Lexemes -----^

^----- Stream  ------
```

---

### Let's jump into some code!

```haskell
-- We generally start off by defining our Parser type synonym
type Parser = Parsec Void Text
                     ^    ^
                     |    |
Custom error component    Type of input (stream)
```

---

```haskell
type Parser = Parsec Void Text

myParser :: Parser MyType
```

---

```haskell
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

/* Here is our parser definition. We will parse text, and have passed in Void for our custom errors, meaning
we'll use the default */
type Parser = Parsec Void String

parseCharH :: Parser Char
parseCharH  = char 'h'

goodInput = "h"

badInput = "not an h"

main = do
    parseTest parseCharH goodInput
    parseTest parseCharH badInput
```

---

```bash
❯❯❯ stack runghc test.hs
'h'
1:1:
unexpected 'n'
expecting 'h'
here
```

---

### A more detailed example: parse TODO's in code

```haskell
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Again, we'll parse strings
type Parser = Parsec Void String
```

---

```haskell

-- a data structure to parse
data TodoEntry = TodoEntry String deriving (Show)
```

---
`Text.Megaparsec.Char`

Offers some basic building blocks for parsing single characters

```haskell
anyChar :: MonadParsec e s m => m (Token s)
digitChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
space :: (MonadParsec e s m, Token s ~ Char) => m ()
```

---

A foundational part of building the parser is deciding on how to handle whitespace, so we can
define our lexeme handling

`Text.Megaparsec.Char.Lexer`

```haskell
-- define our base symbol
symbol    = L.symbol space

-- now that we have a symbol, we can easily write simple, composable, literal parsers 
-- that automatically consume whitespace after
-- for example:
semicolon = symbol ";"
hello = symbol "hello"
parens    = between (symbol "(") (symbol ")")
```

---

```haskell
-- define our base symbol
symbol    = L.symbol space

-- lets parse our TODOs!

haskellCommentStart :: Parser Text
haskellCommentStart = symbol "--"

todoFlag :: Parser Text
todoFlag = symbol "TODO"

-- TODO our parser should be able to parse a line just like this!
parseBasicTODO :: Parser TodoEntry
parseBasicTODO = fail "TODO"
```

---

### The parser monad

At each step in our `do` notation we can:
- consume part of our stream that matches some criteria
  - parse (part or all of) a data type
  - discard it and continue
- inspect our location in the stream without consuming it
  - see `try`, `lookAhead`
- fail

### A more detailed example: parse TODO's in code

```haskell
-- define our base symbol
symbol    = L.symbol space

-- lets parse our TODOs!

haskellCommentStart :: Parser Text
haskellCommentStart = symbol "--"

todoFlag :: Parser Text
todoFlag = symbol "TODO"

parseBasicTODO :: Parser TodoEntry
parseBasicTODO = do
  _ <- try space
  _ <- haskellCommentStart
  _ <- todoFlag
  body <- many anyChar
  return TodoEntry body
```

---

```haskell
parseBasicTODO :: Parser TodoEntry
parseBasicTODO = do
  -- consume any leading space, don't care if it succeeds
  _ <- try space
  -- consume the "--", abort on failure
  _ <- haskellCommentStart
  -- consume "TODO", abort on failure
  _ <- todoFlag
  -- any characters after that gives us the body
  body <- many anyChar
  -- all done!
  return TodoEntry body
```

---

### Running our parser

While developing, running your parser with `parseTest` is nice
for printing results to stdout

```haskell
parseTest
:: (ShowErrorComponent e, Ord (Token s), ShowToken (Token s), Show a)	 
=> Parsec e s a --	Parser to run
-> s -- Input for parser
-> IO ()
```

---

When it's time to deploy your real parser, the most straightforward way to run
you parser is with. (see also: `parseMaybe`, `runParserT`

```haskell
parse
:: Parsec e s a	-- Parser to run
-> String -- Name of source file
-> s -- Input for parser
-> Either (ParseError (Token s) e) a	 
```

---

```haskell
parseBasicTODO :: Parser TodoEntry

main = do
    parseTest parseBasicTODO " -- TODO here's some stuff we need to do!"
    parseTest parseBasicTODO "this should fail"
``` 
--- 

### Let's have some metadata in our todos

Let's say we want to be able to assign TODO's in our code

```haskell
data AssignableTodoEntry = AssignableTodoEntry String (Maybe String) deriving (Show)
```

```
-- TODO(assignee) some todo body text
```

---

```haskell
inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

parseAssignee :: Parser [Char]
parseAssignee = inParens (many $ noneOf [')', '('])

parseTODO :: Parser AssignableTodoEntry
parseTODO = do
  _ <- try space
  _ <- haskellCommentStart
  _ <- todoFlag
  assignee <- optional parseAssignee
  body <- many anyChar
  return $ AssignableTodoEntry body assignee
```

---

```haskell
main = do
  parseTest parseTODO " -- TODO(avi) here's some stuff we need to do!"
  parseTest parseTODO " -- TODO here's some stuff we need to do!"
  parseTest parseTODO "this should fail"
```

```
❯❯❯ stack runghc test1.hs
AssignableTodoEntry "here's some stuff we need to do!" (Just "avi")
AssignableTodoEntry "here's some stuff we need to do!" Nothing
1:1:
unexpected "th"
expecting "--" or white space
```

---

### Falling back

Suppose we want to pasre TODO's or a comment when TODO parsing fails

```haskell
data Comment = TodoEntry String | PlainComment String deriving (Show)
```

---

```haskell
parsePlainComment :: Parser Comment
parsePlainComment = do
  _ <- try space
  _ <- haskellCommentStart
  body <- many anyChar
  return $ PlainComment body
  
-- same implenation as before, just an edited type signature
parseTODO :: Parser Comment

parseComment :: Parser Comment
parseComment = (try parseTODO) <|> parsePlainComment
```

--- 

### Wrapping up

This approach to parsing:

- Powerful parsers that are highly composable
- Type safe
- Readable
- Is easy to maintain

Reach for Haskell + megaparsec (or similar) the next time you have some parsing to do!

---

# Thank you!

[Slides](https://github.com/aviaviavi/talks/intro-pasers)

#### Avi Press
- [Website](https://avi.press)
- [Twitter](https://twitter.com/avi_press)
- [Github](https://github.com/aviaviavi)