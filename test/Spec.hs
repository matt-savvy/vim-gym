import Test.Hspec
import SM2 (Grade(..), applyScore)

main :: IO ()
main = hspec $ do
    describe "applyScore" $ do
        it "with streak = 0" $ do
            let current = (Grade { streak = 0, score = 2.5, interval = 0 })
            (applyScore 5.0 current) `shouldBe` (Grade { streak = 1, score = 2.6, interval = 1 })
