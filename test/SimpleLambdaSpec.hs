module SimpleLambdaSpec where

import Lib
import Test.Hspec
import Test.QuickCheck
import Test.Invariant
import Debug.Trace

testExp = "(λx.λy.xyy)(λz.z)"

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "η-equivalence" $ do
        it "(λx.x) == (λy.y)" $ do
            let a = parse "(λx.x)"
                b = parse "(λy.y)"
             in etaEquivalent a b `shouldBe` True

        it "(λx.x)(y) == (λa.a)(b)" $ do
            let a = parse "(λx.x)(y)"
                b = parse "(λa.a)(b)"
             in etaEquivalent a b `shouldBe` True

        it "(λx.z)(y) ≠ (λa.a)(b)" $ do
            let a = parse "(λx.z)(y)"
                b = parse "(λa.a)(b)"
             in etaEquivalent a b `shouldBe` False

        it "(λx.xxx)(y) = (λx.xxx)(y)" $ do
            let a = parse "(λx.xxx)(y)"
                b = parse "(λx.xxx)(y)"
             in etaEquivalent a b `shouldBe` True

        it "(λx.xzx)(y) = (λa.aqa)(y)" $ do
            let a = parse "(λx.xzx)(y)"
                b = parse "(λa.aqa)(y)"
             in trace (show (snd . eta newTracker $ a) ++ show (snd . eta newTracker $ b)) $ etaEquivalent a b `shouldBe` True

        it "(λx.λy.x)(T)(F) = (λl.λz.l)(T)(F)" $ do
            let a = parse "(λx.λy.x)(T)(F)"
                b = parse "(λl.λz.l)(T)(F)"
             in etaEquivalent a b `shouldBe` True

        it "(λx.λy.)(T)(F) = (λμ.λν.)(T)(F)" $ do
            let a = parse "(λx.λy.)(T)(F)"
                b = parse "(λμ.λν.)(T)(F)"
             in etaEquivalent a b `shouldBe` True

        it "(λx.λy.xy)(λx.x)(λx.xx) = (λX.λY.XY)(λX.X)(λX.XX)" $ do
            let a = parse "(λx.λy.xy)(λx.x)(λx.xx)"
                b = parse "(λX.λY.XY)(λX.X)(λX.XX)"
             in etaEquivalent a b `shouldBe` True

    describe "evalS" $ do
        it "(λx.x) -> (λx.x)" $ do
            evalLoopS "(λx.x)" `shouldBe` [Lambda 'x' (Body [Var 'x'])]

        it "(λx.x)(y) -> y" $ do
            evalLoopS "(λx.x)(y)" `shouldBe` [Var 'y']

        it "(λx.xx)(y) -> y y" $ do
            evalLoopS "(λx.xx)(y)" `shouldBe` [Var 'y', Var 'y']

        it "(λx.xxx)(y) -> y y y" $ do
            evalLoopS "(λx.xxx)(y)" `shouldBe` [Var 'y', Var 'y', Var 'y']

        it "(λx.xzx)(y) -> y z y" $ do
            evalLoopS "(λx.xzx)(y)" `shouldBe` [Var 'y', Var 'z', Var 'y']

        it "(λx.λy.x)(T)(F) -> T" $ do
            evalLoopS "(λx.λy.x)(T)(F)" `shouldBe` [Var 'T']

        it "(λx.λy.)(T)(F) -> ()" $ do
            evalLoopS "(λx.λy.)(T)(F)" `shouldBe` []

        it "(λx.λy.xy)(λx.x)(λx.xx) -> (λx.xx)" $ do
            evalLoopS "(λx.λy.xy)(λx.x)(λx.xx)" `shouldBe` [Lambda 'x' (Body [Var 'x', Var 'x'])]
