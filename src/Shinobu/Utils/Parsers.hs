module Shinobu.Utils.Parsers (InlineCode (..), CodeBlock (..), Code (..), TimeSpan (..), RegExp (..)) where

import Calamity.Commands as CC
import CalamityCommands.ParameterInfo
import Data.Char (isAlphaNum)
import Data.Time
import qualified Polysemy as P
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Text.RE.TDFA as TDFA

choiceBacktrack :: MonadParsec e s f => [f a] -> f a
choiceBacktrack [] = empty
choiceBacktrack [x] = x
choiceBacktrack (x : xs) = try x <|> choiceBacktrack xs

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
  parameterDescription = "code block (multiline or inline)"
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
  choiceBacktrack [Left <$> inlineCode, Right <$> codeBlock] <&> \case
    Left inlineCode_ -> Code "" (inlineCode_ ^. #code)
    Right codeBlock_ -> Code (codeBlock_ ^. #lang) (codeBlock_ ^. #code)

newtype TimeSpan = TimeSpan {fromTimeSpan :: NominalDiffTime}
  deriving newtype (Num)
  deriving stock (Show, Generic)

instance ParameterParser TimeSpan c r where
  parameterDescription = "time span"
  parse = parseMP (parserName @TimeSpan) timeP

timeP :: MonadParsec e Text f => f TimeSpan
timeP =
  choiceBacktrack
    [ decimal <* chunk "s",
      (* 60) <$> decimal <* chunk "m",
      (* (60 * 60)) <$> decimal <* chunk "h",
      (* (24 * 60 * 60)) <$> decimal <* chunk "d",
      (* (7 * 24 * 60 * 60)) <$> decimal <* chunk "d"
    ]

newtype RegExp = RegExp {getTDFA :: TDFA.RE}

instance (P.Embed IO :> r) => ParameterParser RegExp c r where
  parameterDescription = "POSIX regular expression"
  parse = CC.parse @Text >>= (P.embed . fmap RegExp . TDFA.compileRegex . toString)
