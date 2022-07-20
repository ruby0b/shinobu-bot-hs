module Shinobu.Parsers (InlineCode (..), CodeBlock (..), Code (..)) where

import Calamity.Commands
import CalamityCommands.ParameterInfo
import Data.Char (isAlphaNum)
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer

parserName :: forall a c r. ParameterParser a c r => Text
parserName =
  let ParameterInfo (fromMaybe "" -> name) type_ _ = parameterInfo @a @c @r
   in name <> ":" <> fromString (show type_)

newtype InlineCode = InlineCode {code :: Text}
  deriving stock (Show, Generic)

data CodeBlock = CodeBlock {lang :: Text, code :: Text}
  deriving stock (Show, Generic)

data Code = Code {lang :: Text, code :: Text}
  deriving stock (Show, Generic)

instance ParameterParser InlineCode c r where
  parameterDescription = "inline code surrounded by backticks"
  parse = parseMP (parserName @InlineCode) inlineCode

instance ParameterParser CodeBlock c r where
  parameterDescription = "multiline code block surrounded by triple backticks"
  parse = parseMP (parserName @CodeBlock) codeBlock

instance ParameterParser Code c r where
  parameterDescription = "multiline code block surrounded by triple backticks"
  parse = parseMP (parserName @Code) codeP

inlineCode :: MonadParsec e Text m => m InlineCode
inlineCode = between (chunk "`") (chunk "`") (InlineCode <$> takeWhileP Nothing (/= '`'))

isAlphaNumOrLine :: Char -> Bool
isAlphaNumOrLine c = isAlphaNum c || c == '-' || c == '_'

codeBlockLanguage :: (MonadParsec e s m, Token s ~ Char) => m [Char]
codeBlockLanguage = MP.many (satisfy isAlphaNumOrLine <?> "code language")

codeBlock :: MonadParsec e Text m => m CodeBlock
codeBlock = do
  lang <- fromMaybe "" <$> (chunk "```" *> optional (try (codeBlockLanguage <* newline)))
  code <- manyTill charLiteral (chunk "```")
  return $ CodeBlock (fromString lang) (fromString code)

codeP :: MonadParsec e Text f => f Code
codeP =
  try (Left <$> inlineCode) <|> (Right <$> codeBlock) <&> \case
    Left inlineCode_ -> Code "" (inlineCode_ ^. #code)
    Right codeBlock_ -> Code (codeBlock_ ^. #lang) (codeBlock_ ^. #code)
