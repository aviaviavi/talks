import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

symbol    = L.symbol space

haskellCommentStart :: Parser String
haskellCommentStart = symbol "--"

todoFlag :: Parser String
todoFlag = symbol "TODO"

inParens :: Parser a -> Parser a
inParens = between (symbol "(") (symbol ")")

parseAssignee :: Parser String
parseAssignee = inParens (many $ noneOf [')', '('])

-- parseAssignee :: Parser String
-- parseAssignee = inParens (many $ anyChar)

data Comment = TodoEntry String (Maybe String) | PlainComment String deriving (Show)

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

main = do
  parseTest parseComment "-- TODO a parsed todo"
  parseTest (dbg "parseComment" parseComment) "-- TODO(avi) a parsed, assigned todo"
  parseTest parseComment "-- just a comment"
