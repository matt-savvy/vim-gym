import Test.Hspec
import SM2 (Grade(..), applyScore)

main :: IO ()
main = hspec $ do
    describe "applyScore" $ do
        it "with streak = 0" $ do
            let current = (Grade { streak = 0, score = 2.5, interval = 0 })
            (applyScore 5.0 current) `shouldBe` (Grade { streak = 1, score = 2.6, interval = 1 })
        it "with streak = 1" $ do
            let current = (Grade { streak = 1, score = 2.5, interval = 0 })
            (applyScore 5.0 current) `shouldBe` (Grade { streak = 2, score = 2.6, interval = 6 })
        it "with streak = 3" $ do
            let current = (Grade { streak = 3, score = 2.5, interval = 6 })
            (applyScore 4.0 current) `shouldBe` (Grade { streak = 4, score = 2.5, interval = 24 })
