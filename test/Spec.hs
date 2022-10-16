import Test.Hspec
import Cook

main :: IO ()
main = hspec $ do
    describe "Cook" $ do 
        it "returns foo" $ do
            parseCook "foo" `shouldBe` Right "foo"
