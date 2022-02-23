module Error where

import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

type ParseError = ParseErrorBundle String Void

data TypeError
  = TypeOfConditionMustBeBool
  | TypeOfArmsMustMatch
  | TypeOfVarNotSpecified
  | TypeArrowExpected
  | TypeAppArgumentMustMatch
  | TypeOfAscriptionMustMatch
  deriving (Eq, Show)

data CompilerError = ParserError ParseError | TypecheckerError TypeError
  deriving (Eq, Show)
