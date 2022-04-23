module Error where

import Data.Void (Void)
import Kind (Kind)
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
  | TypeTyAppMustFollowKind Type Kind
  | InvalidProjection
  | ProjectionNotAppliedToATuple
  | ProperTypeExpected
  | ArrowKindExpected
  | ParameterKindMismatch
  deriving (Eq, Show)

data CompilerError = ParserError ParseError | TypecheckerError TypeError
  deriving (Eq, Show)
