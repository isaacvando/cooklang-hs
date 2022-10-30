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
                parseCook "this is a step" `shouldBe` (Right $ Recipe [] [[Text "this is a step"]])

            it "step beginning with >" $ do
                parseCook "> sneaky step" `shouldBe` (Right $ Recipe [] [[Text "> sneaky step"]])
            
            it "step beginning with >>>" $ do
                parseCook ">>> sneaky step" `shouldBe` (Right $ Recipe [] [[Text ">>> sneaky step"]])

            it "extra spaces" $ do
                parseCook "    this    is    a step  " `shouldBe` (Right $ Recipe [] [[Text "this is a step"]])

            it "extra tabs and spaces" $ do
                parseCook "\tthis  \t  is    \t\ta step  " `shouldBe` (Right $ Recipe [] [[Text "this is a step"]])

            it "two steps" $ do 
                parseCook "step one\nstep two" `shouldBe` (Right $ Recipe [] [[Text "step one"], [Text "step two"]])

            describe "ingredient" $ do
                it "basic ingredient" $ do
                    parseCook "add @apples" `shouldBe` (Right $ Recipe [] [[Text "add", Ingredient "apples" "" ""]])

                it "ingredient with longer sentence" $ do
                    parseCook "add lots of delicious @apples" `shouldBe` (Right $ Recipe [] [[Text "add lots of delicious", Ingredient "apples" "" ""]])
                
                it "multi word ingredient" $ do
                    parseCook "@honey crisp apples{}" `shouldBe` (Right $ Recipe [] [[Ingredient "honey crisp apples" "" ""]])

                it "extra spacing" $ do
                    parseCook "@  \thoney \t crisp\tapples    {}" `shouldBe` (Right $ Recipe [] [[Ingredient "honey crisp apples" "" ""]])
                
                it "ingredient with quantity" $ do
                    parseCook "@honey crisp apples {a million}" `shouldBe` (Right $ Recipe [] [[Ingredient "honey crisp apples" "a million" ""]])

                it "ingredient with quantity and unit" $ do 
                    parseCook "@chives {10 % bushels}" `shouldBe` (Right $ Recipe [] [[Ingredient "chives" "10" "bushels"]])
                
                it "ingredient with quantity and unit and no spaces" $ do 
                    parseCook "@eggs{0.98%oz}" `shouldBe` (Right $ Recipe [] [[Ingredient "eggs" "0.98" "oz"]])

            describe "cookware" $ do
                it "basic cookware" $ do
                    parseCook "#pot" `shouldBe` (Right $ Recipe [] [[Cookware "pot"]])

                it "multiword cookware" $ do
                    parseCook "#stock pot{}" `shouldBe` (Right $ Recipe [] [[Cookware "stock pot"]])
                
                it "multiword cookware with ingredient" $ do
                    parseCook "# cast iron skillet {blazing hot}" `shouldBe` (Right $ Recipe [] [[Cookware "cast iron skillet"]])

            describe "timer" $ do
                it "basic timer" $ do
                    parseCook "~{10 minutes}" `shouldBe` (Right $ Recipe [] [[Timer "" "10 minutes" ""]])

                it "timer with label" $ do
                    parseCook "~stewed apples {10 minutes}" `shouldBe` (Right $ Recipe [] [[Timer "stewed apples" "10 minutes" ""]])

                it "timer with amount and unit" $ do 
                    parseCook "~grapefruit{2%hours}" `shouldBe` (Right $ Recipe [] [[Timer "grapefruit" "2" "hours"]])

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
                parseCook "i'm a step -- i'm a comment" `shouldBe` (Right $ Recipe [] [[Text "i'm a step"]])

            it "step, inline comment, step" $ do
                parseCook "i'm a step -- i'm a comment\nI'm another step" `shouldBe` (Right $ Recipe [] [[Text "i'm a step"], [Text "I'm another step"]])

            it "ingredient followed by inline comment" $ do
                parseCook "@cayenne pepper{} -- i'm a comment" `shouldBe` (Right $ Recipe [] [[Ingredient "cayenne pepper" "" ""]])

            it "step followed by block comment" $ do
                parseCook "i'm a step [- i'm a comment -]" `shouldBe` (Right $ Recipe [] [[Text "i'm a step"]])
            
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
                parseCook "step one\n>> key: value" `shouldBe` (Right $ Recipe [("key","value")] [[Text "step one"]])


            let fullRecipeText = unlines [   
                    "-- this recipe is delicious!",
                    "[- spinach is optional but a good addition -]",
                    ">> source: https://isaacvando.com",
                    ">> time: 1 hr 20 min",
                    ">> title: Grandma's Quiche",
                    "\n",
                    "Preheat the #oven to 325 degrees.",
                    "In a medium bowl, mix the @eggs{3}, @sour cream{1/2%cup}, @shredded cheese{1/3%cup}, @crushed french fried onions{1/3%cup}, and @chopped spinach{}.",
                    "Mix ingredients thoroughly and add to the @pie crust{1}.",
                    "\t\t\t\t\n\n\n",
                    "Bake for an ~{hour} or until the top is nicely brown and a toothpick comes out clean.",
                    "Serve with @fruit and enjoy!",
                    "\n\n",
                    "[- the",
                    "end -]"
                    ]

            let fullRecipe = Recipe [("source", "https://isaacvando.com"), ("time", "1 hr 20 min"), ("title", "Grandma's Quiche")] 
                        [[Text "Preheat the", Cookware "oven", Text "to 325 degrees."]
                        , [Text "In a medium bowl, mix the", Ingredient "eggs" "3" "", Text ",", Ingredient "sour cream" "1/2" "cup",
                            Text ",", Ingredient "shredded cheese" "1/3" "cup", Text ",", Ingredient "crushed french fried onions" "1/3" "cup",
                            Text ", and", Ingredient "chopped spinach" "" "", Text "."]
                        , [Text "Mix ingredients thoroughly and add to the", Ingredient "pie crust" "1" "", Text "."]
                        , [Text "Bake for an", Timer "" "hour" "", Text "or until the top is nicely brown and a toothpick comes out clean."]
                        , [Text "Serve with", Ingredient "fruit" "" "", Text "and enjoy!"]]

            it "full recipe" $ do
                parseCook fullRecipeText `shouldBe` (Right fullRecipe)

                

