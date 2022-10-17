import Test.Hspec
import Cook

main :: IO ()
main = hspec $ do
    describe "Cook" $ do 
        describe "metadata" $ do
            it "basic metadata" $ do
                (parseCook ">> source: https://isaacvando.com") `shouldBe` (Right $ Recipe [("source", "https://isaacvando.com")] [])
            
            it "two metadata" $ do
                (parseCook ">> first: metadata\n>> second: metadata") `shouldBe` (Right $ Recipe [("first", "metadata"), ("second", "metadata")] [])

            it "simple extra spacing" $ do
                parseCook ">>      \t  key:        value   \t\t\t\t   \n" `shouldBe` (Right $ Recipe [("key", "value")] [])

            it "multiple words with extra spaces" $ do
                parseCook ">>   key:    this    is  the value     " `shouldBe` (Right $ Recipe [("key", "this is the value")] [])
        
        describe "step" $ do
            it "basic step" $ do
                parseCook "this is a step" `shouldBe` (Right $ Recipe [] ["this is a step"])
