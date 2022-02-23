import Data.List
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
      testCase "Arrow type" $
        assertEqual
          []
          (compile "(\\x: Unit -> Bool -> Bool . x unit true)")
          ( Right
              ( Abs "x" (TyArrow TyUnit (TyArrow TyBool TyBool)) (App (App (Var 0 1) TmUnit) TmTrue),
                TyArrow (TyArrow TyUnit (TyArrow TyBool TyBool)) TyBool,
                Abs "x" (TyArrow TyUnit (TyArrow TyBool TyBool)) (App (App (Var 0 1) TmUnit) TmTrue)
              )
          ),
      testCase
        "Sequencing"
        $ assertEqual
          []
          (compile "unit;true")
          ( Right
              ( App (Abs "_" TyUnit TmTrue) TmUnit,
                TyBool,
                TmTrue
              )
          ),
      testCase
        "Ascription with base type"
        $ assertEqual
          []
          (compile "true as Bool")
          ( Right
              ( TmAscription TmTrue TyBool,
                TyBool,
                TmTrue
              )
          ),
      testCase
        "Ascription with complex type"
        $ assertEqual
          []
          (compile "(\\x:Bool.\\y:Bool.y) as Bool->Bool->Bool")
          ( Right
              ( TmAscription (Abs "x" TyBool (Abs "y" TyBool (Var 0 2))) (TyArrow TyBool (TyArrow TyBool TyBool)),
                TyArrow TyBool (TyArrow TyBool TyBool),
                Abs "x" TyBool (Abs "y" TyBool (Var 0 2))
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
          ),
      testCase
        "Tuple expression"
        $ assertEqual
          []
          (compile "{true, \\x:Bool.x}")
          ( Right
              ( TmTuple [TmTrue, Abs "x" TyBool (Var 0 1)],
                TyTuple [TyBool, TyArrow TyBool TyBool],
                TmTuple [TmTrue, Abs "x" TyBool (Var 0 1)]
              )
          ),
      testCase
        "Tuple type expression"
        $ assertEqual
          []
          (compile "\\x:{Bool, Unit}.x")
          ( Right
              ( Abs "x" (TyTuple [TyBool, TyUnit]) (Var 0 1),
                TyArrow (TyTuple [TyBool, TyUnit]) (TyTuple [TyBool, TyUnit]),
                Abs "x" (TyTuple [TyBool, TyUnit]) (Var 0 1)
              )
          ),
      testCase
        "Tuple projection expression"
        $ assertEqual
          []
          (compile "{true, false}.1")
          ( Right
              ( TmProj (TmTuple [TmTrue, TmFalse]) 0,
                TyBool,
                TmTrue
              )
          )
    ]