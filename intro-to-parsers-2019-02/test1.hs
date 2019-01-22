import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

singleLetterP :: Parser Char
singleLetterP = char 'h'

goodInput = "h"
badInput = "not an h"

-- define our base symbol
lexeme = L.lexeme space
symbol = L.symbol space

-- lets parse our TODOs!
haskellCommentStart :: Parser String
haskellCommentStart = symbol "--"

todoFlag :: Parser String
todoFlag = symbol "TODO"

-- a data structure to parse
data TodoEntry = TodoEntry String deriving (Show)

parseBasicTODO :: Parser TodoEntry
parseBasicTODO = do
  _ <- try space
  _ <- haskellCommentStart
  _ <- todoFlag
  body <- many anyChar
  return $ TodoEntry body

-- a data structure to parse
data AssignableTodoEntry = AssignableTodoEntry
  String -- body
  (Maybe String) -- assignee
  deriving (Show)

inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

parseAssignee :: Parser String
parseAssignee = inParens (lexeme $ many $ (noneOf [')', '(', ' ']))

parseTODO :: Parser AssignableTodoEntry
parseTODO = do
  _ <- try space
  _ <- haskellCommentStart
  _ <- todoFlag
  assignee <- optional parseAssignee
  body <- many anyChar
  return $ AssignableTodoEntry body assignee

hello = symbol "hello"

greedyParseAssignee = inParens (many anyChar)

main = do
  -- parseTest singleLetterP "hi"
  -- parseTest hello "hello   " -- this works
  -- parseTest hello "   hello" -- this does not
  -- parseTest parseBasicTODO " -- TODO here's some stuff we need to do!"
  -- parseTest parseBasicTODO "this should fail"

  parseTest parseTODO " -- TODO(avi     ) here's some stuff we need to do!"
  -- parseTest parseTODO " -- TODO here's some stuff we need to do!"
  -- parseTest parseTODO "this should fail"

  -- parseTest parseTODO "-- TODO(avi) a parsed, assigned todo"
  -- parseTest parseTODO "-- just a comment"


