## Monadic Parsers for Everyone
### Avi Press

---

### Intro

---

### Contents

- Intro, traditional approaches
- Develop an intuition for parsing in Haskell: monadic parsing, MegaParsec
  - Library overview
  - Examples

---

### What is parsing?

Parse: _analyze (a string or text) into logical syntactic components_

Loosely:

(String | Text | Bytes | YourUnstructuredDataStream) -> YourDataType

---

### What is parsing?

```javascript
> new Date(Date.parse("Feb 24, 2019"))
Sun Feb 24 2019 00:00:00 GMT-0800 (Pacific Standard Time)
```

---

### What we're used to

For simple things, just checking a few conditions directly works fine. For
instance, grabbing content between brackets:

```haskell
s :: Text

... 
if head s == "[" && last s == "]"
then Text.replace "]" "" (Text.replace "[" "" s)
``` 

---

### What we're used to

For simple things, just checking a few conditions directly works fine. For
instance, grabbing content between brackets:

```haskell
s :: Text

... 
if head s == "[" && last s == "]"
then Text.replace "]" "" (Text.replace "[" "" s)
``` 

What about spaces?

```
"[ we_want_to_parse_this_too      ]"
```

--- 

### What we're used to

What about spaces?

```
"[ we_want_to_parse_this_too      ]"
```


It's often compelling to reach for a regex

```
\[\s*(\S+)\s*\]
```

This is fine, but quickly gets complicated and very challenging to read

--- 

### What we're used to

What about spaces?

```
"[ we_want_to_parse_this_too      ]"
```


It's often compelling to reach for a regex

```
\[\s*(\S+)\s*\]
```

This is fine, but quickly gets complicated and very challenging to read

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
type Parser = Parsec Void String
                     ^    ^
                     |    |
Custom error component    Type of input (stream)

myParser :: Parser MyType
```

---

### The parser monad

- The parser monad is a great example to help build the beginning intuation of
monads as _programmable semicolons_. 

- We start with a stream, and a cursor at the beginning of the stream.

At each step in our `do` notation we can:
- consume part of our stream that matches some criteria
  - parse (part or all of) a data type
  - discard it and continue
- inspect our location in the stream without consuming it
  - see `try`, `lookAhead`
- fail

---

```haskell
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- Here is our parser definition. We will parse text, 
-- and have passed in Void for our custom errors, meaning
-- we'll use the default
type Parser = Parsec Void String

parseCharH :: Parser Char
parseCharH = char 'h'

main = do
    parseTest parseCharH "h"
    parseTest parseCharH "hi"
    parseTest parseCharH "not an h"
```

---

```bash
❯❯❯ stack runghc test1.hs
'h'
'h'
1:1:
unexpected 'n'
expecting 'h'
```

---

### A more detailed example: parse TODO's in code

A real world use-case: a tool to manage TODO's in a codebase. (See the actual 
implementation of [Toodles here](https://github.com/aviaviavi/toodles))

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
anyChar :: MonadParsec e s m => m (Token s) -- (NOTE: renamed to `anySingle` in megaparsec 7)
digitChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
space :: (MonadParsec e s m, Token s ~ Char) => m ()
```

---

A foundational part of building the parser is deciding on how to handle whitespace, so we can
define our lexeme handling

```
import qualified Text.Megaparsec.Char.Lexer as L
```

```haskell
-- define our base symbol
symbol    = L.symbol space

-- now that we have a symbol, we can easily write simple,
-- composable, literal parsers that automatically 
-- consume whitespace after for example:
semicolon = symbol ";"
hello = symbol "hello"
parens    = between (symbol "(") (symbol ")")
```

---

### A note about spaces and lexemes

- By convention, we assume a lexeme _starts at the current position, and may have space following it._

```
parseTest hello "hello   " -- this works
parseTest hello "   hello" -- this does not
```

---

```
❯❯❯ stack runghc test1.hs
"hello"
1:1:
unexpected "   he"
expecting "hello"
```

---

```haskell
-- define our base symbol
symbol    = L.symbol space

-- lets parse our TODOs!

haskellCommentStart :: Parser String
haskellCommentStart = symbol "--"

todoFlag :: Parser String
todoFlag = symbol "TODO"

-- TODO our parser should be able to parse a line just like this!
parseBasicTODO :: Parser TodoEntry
parseBasicTODO = fail "TODO"
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
you parser is with: 

```haskell
parse
:: Parsec e s a	-- Parser to run
-> String -- Name of source file
-> s -- Input for parser
-> Either (ParseError (Token s) e) a	 
```

(see also: `parseMaybe`, `runParserT`)

---

```haskell
parseBasicTODO :: Parser TodoEntry

main = do
    parseTest parseBasicTODO " -- TODO here's some stuff we need to do!"
    parseTest parseBasicTODO "this should fail"
``` 
--- 

### Let's parse some metadata in our todos

Let's say we want to be able to assign TODO's in our code

```haskell
data AssignableTodoEntry = AssignableTodoEntry 
  String -- body
  (Maybe String)  -- an assignee
  deriving (Show)
```

```haskell
-- TODO(assignee) some todo body text
```

---

```haskell
inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

parseAssignee :: Parser String
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

Note:

- discuss the greediness of the parser in parens

---

```haskell
main = do
  parseTest parseTODO " -- TODO(avi) here's some stuff we need to do!"
  parseTest parseTODO " -- TODO here's some stuff we need to do!"
  parseTest parseTODO "this should fail"
```

```
❯❯❯ stack runghc test1.hs
AssignableTodoEntry "here's some stuff we need to do!" (Just "a name")
AssignableTodoEntry "here's some stuff we need to do!" Nothing
1:1:
unexpected "th"
expecting "--" or white space
```

---

### Alternatives

The notion of falling back to a parser when another fails is a common need.
Fortunately, our Parser type has an `Alternative` instance.

```haskell
type Parsec e s = ParsecT e s Identity

instance (Ord e, Stream s) => Alternative (ParsecT e s m) where
  empty  = mzero
  (<|>)  = mplus
  
```

```haskell
(something_that_could_fail <|> another_try <|> default)
```

---

Suppose we want to parse TODO's *or* a plain comment when TODO parsing fails

```haskell
data Comment = TodoEntry String (Maybe String) | PlainComment String deriving (Show)
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

### Debugging

- It can be tough to figure out why your parsers are misbehaving

```
dbg
:: (Stream s, ShowToken (Token s), ShowErrorComponent e, Show a)	 
=> String          -- Debugging label
-> ParsecT e s m a -- Parser to debug
-> ParsecT e s m a -- Parser that prints debugging messages
```

---

```haskell
-- dbg "some label" yourParser

λ> parseTest (dbg "parseComment" parseComment) "-- TODO(avi) a parsed, assigned todo"

parseComment> IN: "-- TODO(avi) a parsed, assigned todo"
parseComment> MATCH (COK): "-- TODO(avi) a parsed, assigned todo"
parseComment> VALUE: TodoEntry "a parsed, assigned todo" (Just "avi")

TodoEntry "a parsed, assigned todo" (Just "avi")
```

- COK—"consumed OK". The parser consumed input and succeeded.
- CERR—"consumed error". The parser consumed input and failed.
- EOK—"empty OK". The parser succeeded without consuming input.
- EERR—"empty error". The parser failed without consuming input.

---

### Wrapping up

This approach to parsing gives us:

- Powerful parsers that are highly composable
- Type safety
- Readability
- Easier maintainence

Reach for Haskell + megaparsec (or similar) the next time you have some parsing to do!

---

# Thank you!
#### Avi Press

- [Slides](https://github.com/aviaviavi/talks/intro-pasers)
- [Toodles](https://github.com/aviaviavi/toodles)
- [https://avi.press](https://avi.press)
- [Twitter](https://twitter.com/avi_press)
- [Github](https://github.com/aviaviavi)
