import Data.List
import Data.Map (fromList)
import Data.Ord
import Error
import Eval
import Parser (parse)
import Term
import Test.Tasty
import Test.Tasty.HUnit
import Type
import Typecheck

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

boolId var = Abs var TyBool (Var 0 1)

compile :: String -> Either CompilerError (Term, Type, Term)
compile input = do
  term <- parse input
  ty <- typeCheck term
  let evaluated = eval term
  Right (term, ty, evaluated)

unitTests =
  testGroup
    "Parser"
    [ testCase "Simple function" $
        assertEqual
          []
          (compile "\\x:Bool . x")
          ( Right
              ( boolId "x",
                TyArrow TyBool TyBool,
                boolId "x"
              )
          ),
      testCase "Simple application" $
        assertEqual
          []
          (compile "(\\x:Bool . x) true")
          ( Right
              ( App (boolId "x") TmTrue,
                TyBool,
                TmTrue
              )
          ),
      testCase "Double application" $
        assertEqual
          []
          (compile "(\\x:Bool . \\ y : Bool . y) true false")
          ( Right
              ( App (App (Abs "x" TyBool (Abs "y" TyBool (Var 0 2))) TmTrue) TmFalse,
                TyBool,
                TmFalse
              )
          ),
      testCase "If expression" $
        assertEqual
          []
          (compile "(\\x:Bool . \\ y : Bool . if x then y else false) true false")
          ( Right
              ( App (App (Abs "x" TyBool (Abs "y" TyBool (TmIf (Var 1 2) (Var 0 2) TmFalse))) TmTrue) TmFalse,
                TyBool,
                TmFalse
              )
          ),
      testCase "Nat expression" $
        assertEqual
          []
          (compile "pred succ succ 0")
          ( Right
              ( TmPred (TmSucc (TmSucc TmZero)),
                TyNat,
                TmSucc TmZero
              )
          ),
      testCase "Nat and bool expression" $
        assertEqual
          []
          (compile "if (zero? pred succ 0) then true else false")
          ( Right
              ( TmIf (TmIsZero (TmPred (TmSucc TmZero))) TmTrue TmFalse,
                TyBool,
                TmTrue
              )
          ),
      testCase "Arrow type" $
        assertEqual
          []
          (compile "(\\x: Nat -> Bool -> Bool . (x 0) true)")
          ( Right
              ( Abs "x" (TyArrow TyNat (TyArrow TyBool TyBool)) (App (App (Var 0 1) TmZero) TmTrue),
                TyArrow (TyArrow TyNat (TyArrow TyBool TyBool)) TyBool,
                Abs "x" (TyArrow TyNat (TyArrow TyBool TyBool)) (App (App (Var 0 1) TmZero) TmTrue)
              )
          ),
      testCase
        "Let expression"
        $ assertEqual
          []
          (compile "let not=(\\x:Bool. if x then false else true) in not true")
          ( Right
              ( TmLet "not" (Abs "x" TyBool (TmIf (Var 0 1) TmFalse TmTrue)) (App (Var 0 1) TmTrue),
                TyBool,
                TmFalse
              )
          )
    ]