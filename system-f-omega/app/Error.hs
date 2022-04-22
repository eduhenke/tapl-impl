module Error where

import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)
import Type (Type)

type ParseError = ParseErrorBundle String Void

data TypeError
  = TypeOfConditionMustBeBool
  | TypeOfArmsMustJoin
  | TypeMustBeNat
  | TypeOfVarNotSpecified
  | TypeOfArmsMustMatch
  | TypeArrowExpected
  | TypeAppArgumentMustMatch Type Type
  | TypeTyAppMustApplyToForallType
  | TypeTyAppMustFollowTypeConstraints Type Type
  | InvalidProjection
  | ProjectionNotAppliedToATuple
  deriving (Eq, Show)

data CompilerError = ParserError ParseError | TypecheckerError TypeError
  deriving (Eq, Show)
