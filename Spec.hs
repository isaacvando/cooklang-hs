-- Copyright 2023 Isaac Van Doren

import Test.Hspec ( hspec, describe, it, shouldBe )
import Text.Cook ( parseCook, Content(..), Result(..), Category(..), Item(..) )


main :: IO ()
main = hspec $ do
    describe "TDD" $ do
        describe "metadata" $ do
            it "basic metadata" $ do
                parseCook ">> source: https://isaacvando.com" `shouldBe` Right (Recipe [("source", "https://isaacvando.com")] [])

            it "two metadata" $ do
                parseCook ">> first: metadata\n>> second: metadata" `shouldBe` Right (Recipe [("first", "metadata"), ("second", "metadata")] [])

            it "multi word key" $ do
                parseCook ">> multi word: value" `shouldBe` Right (Recipe [("multi word", "value")] [])

            it "simple extra spacing" $ do
                parseCook ">>      \t  key:        value   \t\t\t\t   \n" `shouldBe` Right (Recipe [("key", "value")] [])

            it "multiple words with extra spaces" $ do
                parseCook ">>   key:    this    is  the value     " `shouldBe` Right (Recipe [("key", "this is the value")] [])

            it "no spaces" $ do
                parseCook ">>foo:bar" `shouldBe` Right (Recipe [("foo", "bar")] [])

        describe "step" $ do
            it "basic step" $ do
                parseCook "this is a step" `shouldBe` Right (Recipe [] [[Text "this is a step"]])

            it "step beginning with >" $ do
                parseCook "> sneaky step" `shouldBe` Right (Recipe [] [[Text "> sneaky step"]])

            it "two steps" $ do
                parseCook "step one\nstep two" `shouldBe` Right (Recipe [] [[Text "step one"], [Text "step two"]])

            it "space around step" $ do
                parseCook "\n\nfoo\n\t" `shouldBe` Right (Recipe [] [[Text "foo"]])

            describe "ingredient" $ do
                it "basic ingredient" $ do
                    parseCook "add @apples" `shouldBe` Right (Recipe [] [[Text "add ", Ingredient "apples" "some" ""]])

                it "ingredient with longer sentence" $ do
                    parseCook "add lots of delicious @apples" `shouldBe` Right (Recipe [] [[Text "add lots of delicious ", Ingredient "apples" "some" ""]])

                it "multi word ingredient" $ do
                    parseCook "@honey crisp apples{}" `shouldBe` Right (Recipe [] [[Ingredient "honey crisp apples" "some" ""]])

                it "extra spacing" $ do
                    parseCook "@  \thoney \t crisp\tapples    {}" `shouldBe` Right (Recipe [] [[Ingredient "honey crisp apples" "some" ""]])

                it "ingredient with quantity" $ do
                    parseCook "@honey crisp apples {a million}" `shouldBe` Right (Recipe [] [[Ingredient "honey crisp apples" "a million" ""]])

                it "ingredient with quantity and unit" $ do
                    parseCook "@chives {10 % bushels}" `shouldBe` Right (Recipe [] [[Ingredient "chives" "10" "bushels"]])

                it "ingredient with quantity and unit and no spaces" $ do
                    parseCook "@eggs{0.98%oz}" `shouldBe` Right (Recipe [] [[Ingredient "eggs" "0.98" "oz"]])

            describe "cookware" $ do
                it "basic cookware" $ do
                    parseCook "#pot" `shouldBe` Right (Recipe [] [[Cookware "pot" "1"]])

                it "multiword cookware" $ do
                    parseCook "#stock pot{}" `shouldBe` Right (Recipe [] [[Cookware "stock pot" "1"]])

                it "cookware with quantity" $ do
                    parseCook "# cast iron skillet {10}" `shouldBe` Right (Recipe [] [[Cookware "cast iron skillet" "10"]])

            describe "timer" $ do
                it "basic timer" $ do
                    parseCook "~{10 minutes}" `shouldBe` Right (Recipe [] [[Timer "" "10 minutes" ""]])

                it "timer with label" $ do
                    parseCook "~stewed apples {10 minutes}" `shouldBe` Right (Recipe [] [[Timer "stewed apples" "10 minutes" ""]])

                it "timer with amount and unit" $ do
                    parseCook "~grapefruit{2%hours}" `shouldBe` Right (Recipe [] [[Timer "grapefruit" "2" "hours"]])

                it "timer without unit" $ do
                    parseCook "~apple" `shouldBe` Right (Recipe [] [[Timer "apple" "" ""]])

            -- I'm not a big fan of this behavior as it loses information and could change the aesthetic qualities of a recipe
            -- if fractions are desired but I am complying with the canonical tests. 
            describe "fractions" $ do
                it "basic no spaces" $ do
                    parseCook "@parsley{1/2%lbs}" `shouldBe` Right (Recipe [] [[Ingredient "parsley" "0.5" "lbs"]])

                it "repeating decimal" $ do
                    parseCook "@food{1/3}" `shouldBe` Right (Recipe [] [[Ingredient "food" "0.33" ""]])

                it "bigger number" $ do
                    parseCook "@grapefruit{9999/5}" `shouldBe` Right (Recipe [] [[Ingredient "grapefruit" "1999.8" ""]])

                it "leading zeros don't count" $ do
                    parseCook "#pot{08/3}" `shouldBe` Right (Recipe [] [[Cookware "pot" "08/3"]])

                it "division by zero doesn't error" $ do
                    parseCook "~foo{10/0}" `shouldBe` Right (Recipe [] [[Timer "foo" "10/0" ""]])

            describe "general step content" $ do
                it "single ingredient followed by timer" $ do
                    parseCook "@food ~timer{10 minutes}" `shouldBe` Right (Recipe [] [[Ingredient "food" "some" "", Text " ", Timer "timer" "10 minutes" ""]])

                it "single cookware followed by ingredient" $ do
                    parseCook "#pan @two foods{}" `shouldBe` Right (Recipe [] [[Cookware "pan" "1", Text " ", Ingredient "two foods" "some" ""]])

                it "spaces for single word ingredient" $ do
                    parseCook "@ apple" `shouldBe` Right (Recipe [] [[Ingredient "apple" "some" ""]])

                it "spaces for single word cookware" $ do
                    parseCook "# pot" `shouldBe` Right (Recipe [] [[Cookware "pot" "1"]])

                it "spaces for single word timer" $ do
                    parseCook "~ banana" `shouldBe` Right (Recipe [] [[Timer "banana" "" ""]])

                it "timer and cookware" $ do
                    parseCook "~apples and bananas in a #pot" `shouldBe` Right (Recipe [] [[Timer "apples" "" "", Text " and bananas in a ", Cookware "pot" "1"]])

        describe "comments" $ do
            it "single line comment" $ do
                parseCook "-- this is a comment" `shouldBe` Right (Recipe [] [])

            it "block comment" $ do
                parseCook "[- single line block comment -]" `shouldBe` Right (Recipe [] [])

            it "multi line block comment" $ do
                parseCook "[- multi line \n block \n comment \n-]" `shouldBe` Right (Recipe [] [])

            it "block comment containing a - and ]" $ do
                parseCook "[- this ] is just - a comment ] with -]" `shouldBe` Right (Recipe [] [])

            it "block comment containing - adjacent to end" $ do
                parseCook "[-- comment --]" `shouldBe` Right (Recipe [] [])

            it "step followed by inline comment" $ do
                parseCook "i'm a step -- i'm a comment" `shouldBe` Right (Recipe [] [[Text "i'm a step "]])

            it "step, inline comment, step" $ do
                parseCook "i'm a step -- i'm a comment\nI'm another step" `shouldBe` Right (Recipe [] [[Text "i'm a step "], [Text "I'm another step"]])

            it "ingredient followed by inline comment" $ do
                parseCook "@cayenne pepper{} -- i'm a comment" `shouldBe` Right (Recipe [] [[Ingredient "cayenne pepper" "some" "", Text " "]])

            it "step followed by block comment" $ do
                parseCook "i'm a step [- i'm a comment -]" `shouldBe` Right (Recipe [] [[Text "i'm a step "]])

            it "metadata followed by inline comment" $ do
                parseCook ">> key: value -- i'm a comment" `shouldBe` Right (Recipe [("key","value")] [])

            it "metadata followed by block comment" $ do
                parseCook ">> key: value [-i'm a comment-]" `shouldBe` Right (Recipe [("key","value")] [])

        describe "general" $ do
            it "empty input" $ do
                parseCook "" `shouldBe` Right (Recipe [] [])

            it "step and metadata" $ do
                parseCook "step one\n>> key: value" `shouldBe` Right (Recipe [("key","value")] [[Text "step one"]])


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
                        [[Text "Preheat the ", Cookware "oven" "1", Text " to 325 degrees."]
                        , [Text "In a medium bowl, mix the ", Ingredient "eggs" "3" "", Text ", ", Ingredient "sour cream" "0.5" "cup",
                            Text ", ", Ingredient "shredded cheese" "0.33" "cup", Text ", ", Ingredient "crushed french fried onions" "0.33" "cup",
                            Text ", and ", Ingredient "chopped spinach" "some" "", Text "."]
                        , [Text "Mix ingredients thoroughly and add to the ", Ingredient "pie crust" "1" "", Text "."]
                        , [Text "Bake for an ", Timer "" "hour" "", Text " or until the top is nicely brown and a toothpick comes out clean."]
                        , [Text "Serve with ", Ingredient "fruit" "some" "", Text " and enjoy!"]]

            it "full recipe" $ do
                parseCook fullRecipeText `shouldBe` Right fullRecipe

        describe "shopping list" $ do
            it "basic" $ do
                parseCook "[Aldi]\napples" `shouldBe` Right (Grouping [Category "Aldi" [Item "apples" ""]])

            it "multiple stores" $ do
                parseCook "[Costco]\nfoo\nbar\n\n[Walmart]\nbaz" `shouldBe` Right (Grouping [Category "Costco" [Item "foo" "", Item "bar" ""], Category "Walmart" [Item "baz" ""]])

            it "markdown link" $ do
                parseCook "[recipes]\n[here](https://isaacvando.com)" `shouldBe` Right (Grouping [Category "recipes" [Item "[here](https://isaacvando.com)" ""]])

            it "synonym" $ do
                parseCook "[fruit]\nstrawberry|berry of the straw" `shouldBe` Right (Grouping [Category "fruit" [Item "strawberry" "berry of the straw"]])

            it "extra space" $ do
                parseCook "[  Dinner  food  ]   \n   \t apples  | NYC   \n\n\n\n  [lunch food \t]  \n  banana  pie  " 
                    `shouldBe` Right (Grouping [Category "Dinner food" [Item "apples" "NYC"], Category "lunch food" [Item "banana pie" ""]])

            it "no item" $ do
                parseCook "[title]" `shouldBe` Right (Grouping [Category "title" []])

    -- generated by GenerateCanonical.hs from canonical.yaml from https://github.com/cooklang/spec/blob/main/tests/canonical.yaml
    describe "Canonical" $ do
        it "BasicDirection" $ do
            parseCook "Add a bit of chilli" `shouldBe` Right (Recipe [] [[Text "Add a bit of chilli"]])

        it "Comments" $ do
            parseCook "-- testing comments" `shouldBe` Right (Recipe [] [])

        it "CommentsAfterIngredients" $ do
            parseCook "@thyme{2%springs} -- testing comments\nand some text" `shouldBe` Right (Recipe [] [[Ingredient "thyme" "2" "springs",Text " "],[Text "and some text"]])

        it "CommentsWithIngredients" $ do
            parseCook "-- testing comments\n@thyme{2%springs}" `shouldBe` Right (Recipe [] [[Ingredient "thyme" "2" "springs"]])

        it "DirectionsWithDegrees" $ do
            parseCook "Heat oven up to 200°C" `shouldBe` Right (Recipe [] [[Text "Heat oven up to 200\176C"]])

        it "DirectionsWithNumbers" $ do
            parseCook "Heat 5L of water" `shouldBe` Right (Recipe [] [[Text "Heat 5L of water"]])

        it "DirectionWithIngrident" $ do
            parseCook "Add @chilli{3%items}, @ginger{10%g} and @milk{1%l}." `shouldBe` Right (Recipe [] [[Text "Add ",Ingredient "chilli" "3" "items",Text ", ",Ingredient "ginger" "10" "g",Text " and ",Ingredient "milk" "1" "l",Text "."]])

        it "EquipmentMultipleWords" $ do
            parseCook "Fry in #frying pan{}" `shouldBe` Right (Recipe [] [[Text "Fry in ",Cookware "frying pan" "1"]])

        it "EquipmentMultipleWordsWithLeadingNumber" $ do
            parseCook "Fry in #7-inch nonstick frying pan{ }" `shouldBe` Right (Recipe [] [[Text "Fry in ",Cookware "7-inch nonstick frying pan" "1"]])

        it "EquipmentMultipleWordsWithSpaces" $ do
            parseCook "Fry in #frying pan{ }" `shouldBe` Right (Recipe [] [[Text "Fry in ",Cookware "frying pan" "1"]])

        it "EquipmentOneWord" $ do
            parseCook "Simmer in #pan for some time" `shouldBe` Right (Recipe [] [[Text "Simmer in ",Cookware "pan" "1",Text " for some time"]])

        it "EquipmentQuantity" $ do
            parseCook "#frying pan{2}" `shouldBe` Right (Recipe [] [[Cookware "frying pan" "2"]])

        it "EquipmentQuantityOneWord" $ do
            parseCook "#frying pan{three}" `shouldBe` Right (Recipe [] [[Cookware "frying pan" "three"]])

        it "EquipmentQuantityMultipleWords" $ do
            parseCook "#frying pan{two small}" `shouldBe` Right (Recipe [] [[Cookware "frying pan" "two small"]])

        it "Fractions" $ do
            parseCook "@milk{1/2%cup}" `shouldBe` Right (Recipe [] [[Ingredient "milk" "0.5" "cup"]])

        it "FractionsInDirections" $ do
            parseCook "knife cut about every 1/2 inches" `shouldBe` Right (Recipe [] [[Text "knife cut about every 1/2 inches"]])

        it "FractionsLike" $ do
            parseCook "@milk{01/2%cup}" `shouldBe` Right (Recipe [] [[Ingredient "milk" "01/2" "cup"]])

        it "FractionsWithSpaces" $ do
            parseCook "@milk{1 / 2 %cup}" `shouldBe` Right (Recipe [] [[Ingredient "milk" "0.5" "cup"]])

        it "IngredientMultipleWordsWithLeadingNumber" $ do
            parseCook "Top with @1000 island dressing{ }" `shouldBe` Right (Recipe [] [[Text "Top with ",Ingredient "1000 island dressing" "some" ""]])

        it "IngredientWithEmoji" $ do
            parseCook "Add some @🧂" `shouldBe` Right (Recipe [] [[Text "Add some ",Ingredient "\129474" "some" ""]])

        it "IngridentExplicitUnits" $ do
            parseCook "@chilli{3%items}" `shouldBe` Right (Recipe [] [[Ingredient "chilli" "3" "items"]])

        it "IngridentExplicitUnitsWithSpaces" $ do
            parseCook "@chilli{ 3 % items }" `shouldBe` Right (Recipe [] [[Ingredient "chilli" "3" "items"]])

        it "IngridentImplicitUnits" $ do
            parseCook "@chilli{3}" `shouldBe` Right (Recipe [] [[Ingredient "chilli" "3" ""]])

        it "IngridentNoUnits" $ do
            parseCook "@chilli" `shouldBe` Right (Recipe [] [[Ingredient "chilli" "some" ""]])

        it "IngridentNoUnitsNotOnlyString" $ do
            parseCook "@5peppers" `shouldBe` Right (Recipe [] [[Ingredient "5peppers" "some" ""]])

        it "IngridentWithNumbers" $ do
            parseCook "@tipo 00 flour{250%g}" `shouldBe` Right (Recipe [] [[Ingredient "tipo 00 flour" "250" "g"]])

        it "IngridentWithoutStopper" $ do
            parseCook "@chilli cut into pieces" `shouldBe` Right (Recipe [] [[Ingredient "chilli" "some" "",Text " cut into pieces"]])

        it "Metadata" $ do
            parseCook ">> sourced: babooshka" `shouldBe` Right (Recipe [("sourced","babooshka")] [])

        it "MetadataBreak" $ do
            parseCook "hello >> sourced: babooshka" `shouldBe` Right (Recipe [] [[Text "hello >> sourced: babooshka"]])

        it "MetadataMultiwordKey" $ do
            parseCook ">> cooking time: 30 mins" `shouldBe` Right (Recipe [("cooking time","30 mins")] [])

        it "MetadataMultiwordKeyWithSpaces" $ do
            parseCook ">>cooking time    :30 mins" `shouldBe` Right (Recipe [("cooking time","30 mins")] [])

        it "MultiLineDirections" $ do
            parseCook "Add a bit of chilli\n\n\nAdd a bit of hummus" `shouldBe` Right (Recipe [] [[Text "Add a bit of chilli"],[Text "Add a bit of hummus"]])

        it "MultipleLines" $ do
            parseCook ">> Prep Time: 15 minutes\n>> Cook Time: 30 minutes" `shouldBe` Right (Recipe [("Prep Time","15 minutes"),("Cook Time","30 minutes")] [])

        it "MultiWordIngrident" $ do
            parseCook "@hot chilli{3}" `shouldBe` Right (Recipe [] [[Ingredient "hot chilli" "3" ""]])

        it "MultiWordIngridentNoAmount" $ do
            parseCook "@hot chilli{}" `shouldBe` Right (Recipe [] [[Ingredient "hot chilli" "some" ""]])

        it "MutipleIngridentsWithoutStopper" $ do
            parseCook "@chilli cut into pieces and @garlic" `shouldBe` Right (Recipe [] [[Ingredient "chilli" "some" "",Text " cut into pieces and ",Ingredient "garlic" "some" ""]])

        it "QuantityAsText" $ do
            parseCook "@thyme{few%springs}" `shouldBe` Right (Recipe [] [[Ingredient "thyme" "few" "springs"]])

        it "QuantityDigitalString" $ do
            parseCook "@water{7 k }" `shouldBe` Right (Recipe [] [[Ingredient "water" "7 k" ""]])

        it "Servings" $ do
            parseCook ">> servings: 1|2|3" `shouldBe` Right (Recipe [("servings","1|2|3")] [])

        it "SlashInText" $ do
            parseCook "Preheat the oven to 200℃/Fan 180°C." `shouldBe` Right (Recipe [] [[Text "Preheat the oven to 200\8451/Fan 180\176C."]])

        it "TimerDecimal" $ do
            parseCook "Fry for ~{1.5%minutes}" `shouldBe` Right (Recipe [] [[Text "Fry for ",Timer "" "1.5" "minutes"]])

        it "TimerFractional" $ do
            parseCook "Fry for ~{1/2%hour}" `shouldBe` Right (Recipe [] [[Text "Fry for ",Timer "" "0.5" "hour"]])

        it "TimerInteger" $ do
            parseCook "Fry for ~{10%minutes}" `shouldBe` Right (Recipe [] [[Text "Fry for ",Timer "" "10" "minutes"]])

        it "TimerWithName" $ do
            parseCook "Fry for ~potato{42%minutes}" `shouldBe` Right (Recipe [] [[Text "Fry for ",Timer "potato" "42" "minutes"]])
            