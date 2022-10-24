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
                parseCook "this is a step" `shouldBe` (Right $ Recipe [] [[("this is a step", Empty)]])

            it "step beginning with >" $ do
                parseCook "> sneaky step" `shouldBe` (Right $ Recipe [] [[("> sneaky step", Empty)]])
            
            it "step beginning with >>>" $ do
                parseCook ">>> sneaky step" `shouldBe` (Right $ Recipe [] [[(">>> sneaky step", Empty)]])

            it "extra spaces" $ do
                parseCook "    this    is    a step  " `shouldBe` (Right $ Recipe [] [[("this is a step", Empty)]])

            it "extra tabs and spaces" $ do
                parseCook "\tthis  \t  is    \t\ta step  " `shouldBe` (Right $ Recipe [] [[("this is a step", Empty)]])

            it "two steps" $ do 
                parseCook "step one\nstep two" `shouldBe` (Right $ Recipe [] [[("step one", Empty)], [("step two", Empty)]])

            describe "annotations" $ do
                it "basic ingredient" $ do
                    parseCook "add @apples" `shouldBe` (Right $ Recipe [] [[("add", Empty), ("apples", Ingredient Nothing)]])

                it "ingredient with longer sentence" $ do
                    parseCook "add lots of delicious @apples" `shouldBe` (Right $ Recipe [] [[("add lots of delicious", Empty), ("apples", Ingredient Nothing)]])
                
                it "multi word ingredient" $ do
                    parseCook "@honey crisp apples{}" `shouldBe` (Right $ Recipe [] [[("honey crisp apples", Ingredient Nothing)]])

                it "extra spacing" $ do
                    parseCook "@  \thoney \t crisp\tapples    {}" `shouldBe` (Right $ Recipe [] [[("honey crisp apples", Ingredient Nothing)]])
                
                it "ingredient with quantity" $ do
                    parseCook "@honey crisp apples {a million}" `shouldBe` (Right $ Recipe [] [[("honey crisp apples", Ingredient $ Just "a million")]])

        describe "general" $ do
            it "empty input" $ do
                parseCook "" `shouldBe` (Right $ Recipe [] [])
            it "step and metadata" $ do
                parseCook "step one\n>> key: value" `shouldBe` (Right $ Recipe [("key","value")] [[("step one", Empty)]])

