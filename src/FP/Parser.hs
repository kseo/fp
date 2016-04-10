module FP.Parser
  ( parseFP
  ) where

import FP.AST
import FP.Value

import Control.Applicative hiding (many, (<|>)) -- conflicts with Parsec
import Text.Parsec (letter, (<|>), digit, many, many1, oneOf, upper, try, ParseError, parse)
import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellStyle
  { P.identStart = letter <|> digit
  , P.identLetter = letter <|> digit
  , P.opStart = oneOf "+-*"
  , P.opLetter = oneOf ""
  , P.reservedOpNames = ["=", "_", "->", ";"]
  , P.reservedNames = ["T", "F", "bu", "while", "if", "Def"]
  })

whiteSpace = P.whiteSpace lexer
natural    = P.natural lexer
lexeme     = P.lexeme lexer
angles     = P.angles lexer
parens     = P.parens lexer
brackets   = P.brackets lexer
identifier = P.identifier lexer
operator   = P.operator lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
colon      = P.colon lexer
commaSep1  = P.commaSep1 lexer

boolAtom :: Parser Atom
boolAtom = (reserved "T" *> return (BoolAtom True))
       <|> (reserved "F" *> return (BoolAtom False))

numberAtom :: Parser Atom
numberAtom = NumberAtom <$> natural

symbolAtom :: Parser Atom
symbolAtom = SymbolAtom <$> many1 upper

atomObject :: Parser Object
atomObject = AtomObject <$> (numberAtom <|> boolAtom <|> symbolAtom)

sequenceObject :: Parser Object
sequenceObject = SequenceObject <$> angles (commaSep1 (atomObject <|> sequenceObject))

object :: Parser Object
object = atomObject <|> sequenceObject

functionList :: Parser [Function]
functionList = brackets (commaSep1 function)

term = parens function
  <|> Function <$> (identifier <|> operator)
  <|> Construction <$> functionList
  <|> Constant <$> (reservedOp "_" *> object)
  <|> BinaryToUnary <$> (reserved "bu" *> function) <*> object
  <|> While <$> (reserved "while" *> function) <*> function
  <|> Condition <$> (reserved "if" *> function <* reservedOp "->") <*> (function <* reservedOp ";") <*> function

table = [ [prefix "@" ApplyToAll, prefix "/" Insert ],
          [binary "." Composition AssocRight]
        ]
          where prefix  name fun = Prefix  (fun <$ reservedOp name)
                binary  name fun = Infix   (fun <$ reservedOp name)

function :: Parser Function
function = buildExpressionParser table term

definition :: Parser Definition
definition = Definition <$> (reserved "Def" *> identifier) <*> (reservedOp "=" *> function)

expression :: Parser Expression
expression = try (Application <$> function <*> (colon *> object)) <|> (Object <$> object)

-- Explicit whiteSpace is needed to skip any leading white space.
program :: Parser Program
program = Program <$> (whiteSpace *> many definition) <*> expression

parseFP :: String -> Either ParseError Program
parseFP = parse program ""
