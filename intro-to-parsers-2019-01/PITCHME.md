## Parsing in Haskell
### Avi Press

---

### Introduction

Note:

- I'm a software engineer at a local startup, Fraction, a platform for digital offers on Google Pay and Apple Pay. 
- Learning Haskell has been generally tough for me.
- Parsing, an often bragged about application of Haskell, was no exception, I had a hard time.
- This talk aims to give an approachable introduction to how I finally got comfortable with it.
- Please ask questions!

---

### Contents

- Intro, traditional approaches
- Develop an intuition for parsing in Haskell: monadic parsing, MegaParsec
  - Library overview
  - Examples
  
Note:

- Focusing on easy Haskell because that's what was easiest for me to understand.

---

### What is parsing?

- *Parse*: _analyze (a string or text) into logical syntactic components_
- Loosely:

```
(String | Text | Bytes | Similar) -> YourDataType
```

---

### What is parsing?

```javascript
> new Date(Date.parse("Feb 24, 2019"))
Sun Feb 24 2019 00:00:00 GMT-0800 (Pacific Standard Time)
```

---

### What we're used to

@css[fragment](- For simple things, just checking a few conditions directly works fine. For instance, grabbing content between brackets)

@css[fragment]

```
[parse_this]
```

@cssend

@css[fragment](
```haskell
s :: Text

... 
if head s == "[" && last s == "]"
then Text.replace "]" "" (Text.replace "[" "" s)
``` 
)

Note:

So, what's the naive approach to this?

---

### What we're used to

- For simple things, just checking a few conditions directly works fine. For
instance, grabbing content between brackets:

```haskell
s :: Text

... 
if head s == "[" && last s == "]"
then Text.replace "]" "" (Text.replace "[" "" s))
```

- What about spaces?

```
"[ we_want_to_parse_this_too      ]"
```

--- 

### What we're used to

- What about spaces?

```
"[ we_want_to_parse_this_too      ]"
```

- It's often compelling to reach for a regex

```
\[\s*(\S+)\s*\]
```

- This is fine, but quickly gets complicated and very challenging to read

--- 

### What we're used to

- What about spaces?

```
"[ we_want_to_parse_this_too      ]"
```


- It's often compelling to reach for a regex

```
\[\s*(\S+)\s*\]
```

- This is fine, but quickly gets complicated and very challenging to read

```
# a regex for parsing dates, good luck editing this after a year passes!
^(?:(?:(?:0?[13578]|1[02])(\/|-|\.)31)\1|(?:(?:0?[13-9]|1[0-2])(\/|-|\.)(?:29|30)\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:0?2(\/|-|\.)29\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:(?:0?[1-9])|(?:1[0-2]))(\/|-|\.)(?:0?[1-9]|1\d|2[0-8])\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$
```

---

### What we're used to

Plain old property checking of strings

@ul
- Pros:
  + Parse however you want
  + As powerful as you care to make it
- Cons:
  + Often ends up being much more code than a simple regex
  + Ad-hoc property checking functions are fine until you need to generalize
  + Eventually end up reinventing the wheel
@ulend

---

### What we're used to

Regular expressions

@ul
- Pros:
  + Declarative
  + Quite powerful
- Cons:
  + Empirically a nightmare
  + Difficult to read, harder to debug
  + Error handling quickly becomes complex, difficult or impossible
@ulend
    
---
    
### Haskell's solution:
#### Monadic parsing and parser combinators

- Small parsing functions and powerful ways to combine them
- For this talk, we'll be using the MegaParsec library.

---

### Some terminology

- *Stream*: The sequence of information we're trying to parse
- *Token*: A single element of the stream
- *Lexeme*: A logical collection of tokens, like a word and the following spaces

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

Note:

- The first thing to define is what we take in, what we will return when
something goes wrong. Then we can parse whatever types we want to throw at it

---

### MonadParsec

```haskell
type Parsec e s = ParsecT e s Identity

data ParsecT e s m a

-- Instances
(Ord e, Stream s) => MonadParsec e s (ParsecT e s m)
...
```

- We start with a stream, and a cursor at the beginning of the stream.
- The MonadParsec handles some book keeping at each step
  - This concretely useful monad can be helpful for building an intution for
    working with monads in general
    
Note:

- The parsec alias we make is an alias of a data type that has an instance of
MonadParsec, a monad that gives all the plumbing to easily write powerful
parsers

---

- At each step in our `do` notation we can:
  - consume part of our stream that matches some criteria
    - parse (part or all of) a data type
    - discard it and continue
  - inspect our location in the stream without consuming it
    - see `try`, `lookAhead`
  - fail
  
Note:

- There are other functions in the complete MonadParsec definition, but these
  are the useful ones to start out and will take you far

---

- Being able to have our parsers interact with any other monadic effects in a
  type safe way is a huge gain from thinking about our parsers this way

Note: 
- We pass in `Identity` for the underlying monad at the end of the
signature. It could easily be another arbitrary monad stack, giving you type
safe effects. Try that with regular expressions!

---

```haskell
-- We generally start off by defining our Parser type synonym
type Parser = Parsec Void String
                     ^    ^
                     |    |
Custom error component    Type of input (stream)

myParser :: Parser MyType
```

Note:

- So again just as a reminder, we're going to start off by defining our Parser
  synonym, and with that, we can define our top level parser function and work
  down

---

```haskell
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

-- We will parse Strings, and have passed in Void for our
-- custom errors, meaning we'll use the default
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

- A real world use-case: a tool to manage TODO's in a codebase. (See the actual 
implementation of [Toodles here](https://github.com/aviaviavi/toodles))

---

```haskell
-- a data structure to parse
data TodoEntry = TodoEntry String deriving (Show)
```

---

`Text.Megaparsec.Char`

Offers some basic building blocks for parsing single characters

```haskell
 -- (NOTE: renamed to `anySingle` in megaparsec 7)
anyChar :: MonadParsec e s m => m (Token s)
digitChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
space :: (MonadParsec e s m, Token s ~ Char) => m ()
```

---

- A foundational part of building the parser is deciding on how to separate our lexemes, ie the whitespace.
- By convention, we assume a lexeme _starts at the head of the stream, and any following whitespace._


```
import qualified Text.Megaparsec.Char.Lexer as L
```

```haskell
-- define our lexeme, in this case L.symbol, for parsing verbatim strings
symbol = L.symbol space

-
semicolon = symbol ";"
hello = symbol "hello"
inParens = between (symbol "(") (symbol ")")
```

Note:

- symbol is a helper function for a specific kind of lexeme: parsing verbatim strings
- it's a special case of L's `lexeme` function, which just has a more general type signature. It takes an arbitrary parser rather than just a string
- `between` is part of parser-combinators, a dependency 

---

### Lexemes and spaces

```
hello = symbol "hello"

-- this works
parseTest hello "hello hello "
-- this does not
parseTest hello "   hello hello"
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
symbol = L.symbol space

-- lets parse our TODOs!

haskellCommentStart :: Parser String
haskellCommentStart = symbol "--"

todoFlag :: Parser String
todoFlag = symbol "TODO"

-- TODO our parser should be able to parse a line just like this!
parseBasicTODO :: Parser TodoEntry
parseBasicTODO = fail "TODO"
```

Note:

- So lets get back to our Todo's

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
  return $ TodoEntry body
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
lexeme = L.lexeme space

inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

parseAssignee :: Parser String
parseAssignee = inParens . lexeme . many $ noneOf [')', '(']

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
AssignableTodoEntry "here's some stuff we need to do!" (Just "avi")
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
parseTODO = do
  _ <- try space
  _ <- haskellCommentStart
  _ <- todoFlag
  assignee <- optional parseAssignee
  body <- many anyChar
  return $ TodoEntry body assignee

parseComment :: Parser Comment
parseComment = (try parseTODO) <|> parsePlainComment
```

---

```haskell
main = do
  parseTest parseComment "-- TODO a parsed todo"
  parseTest parseComment "-- TODO(avi) a parsed, assigned todo"
  parseTest parseComment "-- just a comment"
```

```
❯❯❯ stack runghc alternative.hs
TodoEntry "a parsed todo" Nothing
TodoEntry "a parsed, assigned todo" (Just "avi")
PlainComment "just a comment"
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

---

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

# Thanks!
#### Avi Press

- Talk
  - [Slides](https://gitpitch.com/aviaviavi/talks?p=intro-to-parsers-2019-01)
  - [Toodles Source](https://github.com/aviaviavi/toodles)
- Follow Me!
  - [https://avi.press](https://avi.press)
  - [Twitter](https://twitter.com/avi_press)
  - [Github](https://github.com/aviaviavi)
