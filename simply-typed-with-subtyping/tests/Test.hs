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
              ( TmRecord . fromList $ [("1", TmTrue), ("2", Abs "x" TyBool (Var 0 1))],
                TyRecord . fromList $ [("1", TyBool), ("2", TyArrow TyBool TyBool)],
                TmRecord . fromList $ [("1", TmTrue), ("2", Abs "x" TyBool (Var 0 1))]
              )
          ),
      testCase
        "Tuple type expression"
        $ assertEqual
          []
          (compile "\\x:{Bool, Unit}.x")
          ( Right
              ( Abs "x" (TyRecord . fromList $ [("1", TyBool), ("2", TyUnit)]) (Var 0 1),
                TyArrow (TyRecord . fromList $ [("1", TyBool), ("2", TyUnit)]) (TyRecord . fromList $ [("1", TyBool), ("2", TyUnit)]),
                Abs "x" (TyRecord . fromList $ [("1", TyBool), ("2", TyUnit)]) (Var 0 1)
              )
          ),
      testCase
        "Tuple projection expression"
        $ assertEqual
          []
          (compile "{true, false}.1")
          ( Right
              ( TmProj (TmRecord . fromList $ [("1", TmTrue), ("2", TmFalse)]) "1",
                TyBool,
                TmTrue
              )
          ),
      testCase
        "Record expression"
        $ assertEqual
          []
          (compile "{first=true, second=false}")
          ( Right
              ( TmRecord $ fromList [("first", TmTrue), ("second", TmFalse)],
                TyRecord $ fromList [("first", TyBool), ("second", TyBool)],
                TmRecord $ fromList [("first", TmTrue), ("second", TmFalse)]
              )
          ),
      testCase
        "Record projection expression"
        $ assertEqual
          []
          (compile "{first=true, second=false}.second")
          ( Right
              ( TmProj (TmRecord $ fromList [("first", TmTrue), ("second", TmFalse)]) "second",
                TyBool,
                TmFalse
              )
          ),
      testCase
        "Record type expression"
        $ assertEqual
          []
          (compile "\\x:{first:Bool}.true")
          ( Right
              ( Abs "x" (TyRecord . fromList $ [("first", TyBool)]) TmTrue,
                TyArrow (TyRecord . fromList $ [("first", TyBool)]) TyBool,
                Abs "x" (TyRecord . fromList $ [("first", TyBool)]) TmTrue
              )
          ),
      testCase
        "Record projection on inexistent label fails typechecking"
        $ assertEqual
          []
          (compile "{first=true, second=false}.third")
          (Left $ TypecheckerError InvalidProjection),
      testCase
        "Record projection with abstraction expression"
        $ assertEqual
          []
          (compile "(\\x:Bool.{a=x}) true")
          ( Right
              ( App (Abs "x" TyBool (TmRecord . fromList $ [("a", Var 0 1)])) TmTrue,
                TyRecord . fromList $ [("a", TyBool)],
                TmRecord . fromList $ [("a", TmTrue)]
              )
          ),
      testCase
        "Variant expression"
        $ assertEqual
          []
          (parse "<a=true> as <a:Bool,b:Bool>")
          ( Right
              ( TmVariant TmTrue "a" (TyVariant . fromList $ [("a", TyBool), ("b", TyBool)])
              )
          ),
      testCase
        "Case expression"
        $ assertEqual
          []
          (compile "case <b=\\x:Unit.true> as <a:Bool,b:Unit->Bool> of <a=x> => x, <b=x> => x unit")
          ( Right
              ( TmCase (TmVariant (Abs "x" TyUnit TmTrue) "b" (TyVariant . fromList $ [("a", TyBool), ("b", TyArrow TyUnit TyBool)])) (fromList $ [("a", ("x", Var 0 1)), ("b", ("x", App (Var 0 1) TmUnit))]),
                TyBool,
                TmTrue
              )
          ),
      testCase
        "Fix expression"
        $ assertEqual
          []
          (compile "fix \\x:Bool.true")
          ( Right
              ( TmFix (Abs "x" TyBool TmTrue),
                TyBool,
                TmTrue
              )
          ),
      testCase
        "Fix with let and nat expression"
        $ assertEqual
          []
          ( compile
              "\
              \let iseven=(fix (\
              \    \\ie:Nat->Bool.\
              \      \\x:Nat.\
              \         if (iszero x) then true\
              \         else (if (iszero (pred x))\
              \         then false else ie (pred (pred x)))))\
              \in iseven (succ succ 0)"
          )
          ( Right
              ( TmLet
                  "iseven"
                  ( TmFix $
                      Abs "ie" (TyArrow TyNat TyBool) $
                        Abs "x" TyNat $
                          TmIf (TmIsZero (Var 0 2)) TmTrue $
                            TmIf
                              (TmIsZero (TmPred (Var 0 2)))
                              TmFalse
                              (App (Var 1 2) (TmPred (TmPred (Var 0 2))))
                  )
                  (App (Var 0 1) (TmSucc (TmSucc TmZero))),
                TyBool,
                TmTrue
              )
          )
    ]