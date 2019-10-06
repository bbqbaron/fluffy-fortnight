{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map         as M
import           Lib
import           Parse
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Tests"
 [unitTests]

t31 :: Inference -> Mono
t31 (Right (_,x,_)) = x
t31 (Left s)        = Bound $ "Err: " ++ s

unitTests :: TestTree
unitTests = testGroup "Unit"
 [testCase "Parse"
 $ assertEqual ""
 (toExpr $ Val "a")
 (Ref $ Symbol "a"),
  testCase "PCall" $
  assertEqual ""
  (parse "$hi there^")
  [Node [Val "hi", Val "there"]]
  , testCase "Call"
  $ assertEqual ""
  (Call (Lam (Symbol "a") $ (Ref $ Symbol "b")) (Ref $ Symbol "x"))
  $ toExpr $ head $ parse "$$fn a b^ x^"
  ,
   testCase "Ref"
  $ assertEqual ""
  (Bound "a")
   (t31 $ infer
          (-1, (M.fromList [(Ref $ Symbol "b", pbound "a")]))
          $ Let (Symbol "a") (Ref $ Symbol "b") (Ref $ Symbol "a")),
   testCase "Let"
   $ assertEqual ""
   (Bound "a")
   (t31 $
   infer (-1, (M.fromList
          [(Ref $ Symbol "b", pbound "a")]))
     (toExpr $ head $ parse "$let a b a^")
   ),
   testCase "Fun"
   $ assertEqual ""
   (fntype [Var 0, Bound "d"])
   $ t31 (infer
         (-1, (M.fromList
         [(Ref $ Symbol "b", pbound "d")]))
         (toExpr $ head $ parse "$fn p b^"))
    , testCase "Polymorphic Fun"
   $ assertEqual ""
   (fntype [Var 0, Var 0])
   $ t31 (infer
         (-1, (M.fromList
         [(Ref $ Symbol "b", pbound "d")]))
         (toExpr $ head $ parse "$fn p p^"))

 ]


main :: IO ()
main = defaultMain tests
