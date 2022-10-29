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
                parseCook "this is a step" `shouldBe` (Right $ Recipe [] [[("this is a step", Text)]])

            it "step beginning with >" $ do
                parseCook "> sneaky step" `shouldBe` (Right $ Recipe [] [[("> sneaky step", Text)]])
            
            it "step beginning with >>>" $ do
                parseCook ">>> sneaky step" `shouldBe` (Right $ Recipe [] [[(">>> sneaky step", Text)]])

            it "extra spaces" $ do
                parseCook "    this    is    a step  " `shouldBe` (Right $ Recipe [] [[("this is a step", Text)]])

            it "extra tabs and spaces" $ do
                parseCook "\tthis  \t  is    \t\ta step  " `shouldBe` (Right $ Recipe [] [[("this is a step", Text)]])

            it "two steps" $ do 
                parseCook "step one\nstep two" `shouldBe` (Right $ Recipe [] [[("step one", Text)], [("step two", Text)]])

            describe "ingredient" $ do
                it "basic ingredient" $ do
                    parseCook "add @apples" `shouldBe` (Right $ Recipe [] [[("add", Text), ("apples", Ingredient Nothing)]])

                it "ingredient with longer sentence" $ do
                    parseCook "add lots of delicious @apples" `shouldBe` (Right $ Recipe [] [[("add lots of delicious", Text), ("apples", Ingredient Nothing)]])
                
                it "multi word ingredient" $ do
                    parseCook "@honey crisp apples{}" `shouldBe` (Right $ Recipe [] [[("honey crisp apples", Ingredient Nothing)]])

                it "extra spacing" $ do
                    parseCook "@  \thoney \t crisp\tapples    {}" `shouldBe` (Right $ Recipe [] [[("honey crisp apples", Ingredient Nothing)]])
                
                it "ingredient with quantity" $ do
                    parseCook "@honey crisp apples {a million}" `shouldBe` (Right $ Recipe [] [[("honey crisp apples", Ingredient $ Just ("a million", Nothing))]])

            describe "cookware" $ do
                it "basic cookware" $ do
                    parseCook "#pot" `shouldBe` (Right $ Recipe [] [[("pot", Cookware "pot")]])

                it "multiword cookware" $ do
                    parseCook "#stock pot{}" `shouldBe` (Right $ Recipe [] [[("stock pot", Cookware "stock pot")]])
                
                it "multiword cookware with ingredient" $ do
                    parseCook "# cast iron skillet {blazing hot}" `shouldBe` (Right $ Recipe [] [[("cast iron skillet", Cookware "cast iron skillet")]])

            describe "timer" $ do
                it "basic timer" $ do
                    parseCook "~{10 minutes}" `shouldBe` (Right $ Recipe [] [[("10 minutes", Timer "")]])

                it "timer with label" $ do
                    parseCook "~stewed apples {10 minutes}" `shouldBe` (Right $ Recipe [] [[("10 minutes", Timer "stewed apples")]])

        describe "comments" $ do
            it "single line comment" $ do
                parseCook "-- this is a comment" `shouldBe` (Right $ Recipe [] [])

            it "block comment" $ do
                parseCook "[- single line block comment -]" `shouldBe` (Right $ Recipe [] [])

            it "multi line block comment" $ do
                parseCook "[- multi line \n block \n comment \n-]" `shouldBe` (Right $ Recipe [] [])

            it "block comment containing a - and ]" $ do
                parseCook "[- this ] is just - a comment ] with -]" `shouldBe` (Right $ Recipe [] [])

            it "block comment containing - adjacent to end" $ do 
                parseCook "[-- comment --]" `shouldBe` (Right $ Recipe [] [])

            it "step followed by inline comment" $ do
                parseCook "i'm a step -- i'm a comment" `shouldBe` (Right $ Recipe [] [[("i'm a step", Text)]])

            it "step, inline comment, step" $ do
                parseCook "i'm a step -- i'm a comment\nI'm another step" `shouldBe` (Right $ Recipe [] [[("i'm a step", Text)], [("I'm another step", Text)]])

            it "ingredient followed by inline comment" $ do
                parseCook "@cayenne pepper{} -- i'm a comment" `shouldBe` (Right $ Recipe [] [[("cayenne pepper", Ingredient Nothing)]])

            it "step followed by block comment" $ do
                parseCook "i'm a step [- i'm a comment -]" `shouldBe` (Right $ Recipe [] [[("i'm a step", Text)]])
            
            it "metadata followed by inline comment" $ do
                parseCook ">> key: value -- i'm a comment" `shouldBe` (Right $ Recipe [("key","value")] [])

            it "metadata followed by block comment" $ do
                parseCook ">> key: value [-i'm a comment-]" `shouldBe` (Right $ Recipe [("key","value")] [])

            -- it "metadata followed by adjacent block comment" $ do
            --     parseCook ">> key: value[-i'm a comment-]" `shouldBe` (Right $ Recipe [("key","value")] [])

        describe "general" $ do
            it "empty input" $ do
                parseCook "" `shouldBe` (Right $ Recipe [] [])
            it "step and metadata" $ do
                parseCook "step one\n>> key: value" `shouldBe` (Right $ Recipe [("key","value")] [[("step one", Text)]])

