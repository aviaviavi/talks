## Monadic Parsers for Imperative Programmers
### Avi Press

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

`[ we_want_to_parse_this_too      ]`

---

### What we're used to

What about spaces?

`[ we_want_to_parse_this_too      ]`


It's often compelling to reach for a regex

```
\[\s*(\S+)\s*\]
```

This is fine, but quickly gets complicated and very challenging to read:

```
# good luck editing this in 6 months!
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
{-# LANGUAGE OverloadedStrings #-}

import           Data.Text            (Text)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

/* Here is our parser definition. We will parse text, and have passed in Void for our custom errors, meaning
we'll use the default */
type Parser = Parsec Void Text

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
~/s/t/intro-to-parsers-and-cr-2018-08 ❯❯❯ stack runghc test.hs
'h'
1:1:
unexpected 'n'
expecting 'h'
here
```

---

### A more detailed example: parse TODO's in code

```haskell
{-# LANGUAGE OverloadedStrings #-}

import           Data.Text            (Text)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Again, we'll parse some text
type Parser = Parsec Void Text
```

---

### A more detailed example: parse TODO's in code

```haskell

-- a data structure to parse
data TodoEntry = TodoEntry { body :: Text }
```

---

### A more detailed example: parse TODO's in code

`Text.Megaparsec.Char`

Offers some basic building blocks for parsing single characters

```haskell
anyChar :: MonadParsec e s m => m (Token s)
digitChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
space :: (MonadParsec e s m, Token s ~ Char) => m ()
```

---

### A more detailed example: parse TODO's in code

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

### A more detailed example: parse TODO's in code

```haskell
-- define our base symbol
symbol    = L.symbol space

-- lets parse our TODOs!

haskellCommentStart :: Parser Text
haskellCommentStart = symbol "--"

todoFlag :: Parser Text
todoFlag = symbol "TODO"

parseTODO :: Parser TodoEntry
parseTODO = fail "TODO"
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

parseTODO :: Parser TodoEntry
parseTODO = do
  _ <- optional space
  _ <- haskellCommentStart
  _ <- todoFlag
  body <- many anyChar
  return TodoEntry body
```
