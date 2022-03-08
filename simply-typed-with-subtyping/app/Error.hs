module Error where

import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

type ParseError = ParseErrorBundle String Void

data TypeError
  = TypeOfConditionMustBeBool
  | TypeOfArmsMustJoin
  | TypeMustBeNat
  | TypeOfVarNotSpecified
  | TypeArrowExpected
  | TypeAppArgumentMustMatch
  | TypeOfAscriptionMustMatch
  | ProjectionNotAppliedToARecord
  | InvalidProjection
  | TypeOfVariantMustMatch
  | MustHaveAscriptionOfVariantType
  | CaseMustBeAppliedToVariant
  | NonMatchingBranchesOfCaseWithVariantType
  | CaseBranchesTypesMustMatchVariantType
  | FixMustBeAppliedToAnArrowTypeWithSameDomainAndImage
  deriving (Eq, Show)

data CompilerError = ParserError ParseError | TypecheckerError TypeError
  deriving (Eq, Show)
