--Canonical Tests for the Cooklang project
import Test.Hspec
import Cook

main :: IO ()
main = hspec $ do
    describe "Canonical" $ do
        it "BasicDirection" $ do
            parseCook "Add a bit of chilli" `shouldBe` (Right $ Recipe [][[Text "Add a bit of chilli"]])

        it "Comments" $ do
            parseCook "-- testing comments" `shouldBe` (Right $ Recipe [][[]])

        it "CommentsAfterIngredients" $ do
            parseCook "@thyme{2%springs} -- testing comments\nand some text" `shouldBe` (Right $ Recipe [][[Ingredient "thyme" "2" "springs",Text " "],[Text "and some text"]])

        it "CommentsWithIngredients" $ do
            parseCook "-- testing comments\n@thyme{2%springs}" `shouldBe` (Right $ Recipe [][[Ingredient "thyme" "2" "springs"]])

        it "DirectionsWithDegrees" $ do
            parseCook "Heat oven up to 200°C" `shouldBe` (Right $ Recipe [][[Text "Heat oven up to 200\176C"]])

        it "DirectionsWithNumbers" $ do
            parseCook "Heat 5L of water" `shouldBe` (Right $ Recipe [][[Text "Heat 5L of water"]])

        it "DirectionWithIngrident" $ do
            parseCook "Add @chilli{3%items}, @ginger{10%g} and @milk{1%l}." `shouldBe` (Right $ Recipe [][[Text "Add ",Ingredient "chilli" "3" "items",Text ", ",Ingredient "ginger" "10" "g",Text " and ",Ingredient "milk" "1" "l",Text "."]])

        it "EquipmentMultipleWords" $ do
            parseCook "Fry in #frying pan{}" `shouldBe` (Right $ Recipe [][[Text "Fry in ",Cookware "frying pan" "1"]])

        it "EquipmentMultipleWordsWithLeadingNumber" $ do
            parseCook "Fry in #7-inch nonstick frying pan{ }" `shouldBe` (Right $ Recipe [][[Text "Fry in ",Cookware "7-inch nonstick frying pan" "1"]])

        it "EquipmentMultipleWordsWithSpaces" $ do
            parseCook "Fry in #frying pan{ }" `shouldBe` (Right $ Recipe [][[Text "Fry in ",Cookware "frying pan" "1"]])

        it "EquipmentOneWord" $ do
            parseCook "Simmer in #pan for some time" `shouldBe` (Right $ Recipe [][[Text "Simmer in ",Cookware "pan" "1",Text " for some time"]])

        it "EquipmentQuantity" $ do
            parseCook "#frying pan{2}" `shouldBe` (Right $ Recipe [][[Cookware "frying pan" "2"]])

        it "EquipmentQuantityOneWord" $ do
            parseCook "#frying pan{three}" `shouldBe` (Right $ Recipe [][[Cookware "frying pan" "three"]])

        it "EquipmentQuantityMultipleWords" $ do
            parseCook "#frying pan{two small}" `shouldBe` (Right $ Recipe [][[Cookware "frying pan" "two small"]])

        it "Fractions" $ do
            parseCook "@milk{1/2%cup}" `shouldBe` (Right $ Recipe [][[Ingredient "milk" "0.5" "cup"]])

        it "FractionsInDirections" $ do
            parseCook "knife cut about every 1/2 inches" `shouldBe` (Right $ Recipe [][[Text "knife cut about every 1/2 inches"]])

        it "FractionsLike" $ do
            parseCook "@milk{01/2%cup}" `shouldBe` (Right $ Recipe [][[Ingredient "milk" "01/2" "cup"]])

        it "FractionsWithSpaces" $ do
            parseCook "@milk{1 / 2 %cup}" `shouldBe` (Right $ Recipe [][[Ingredient "milk" "0.5" "cup"]])

        it "IngredientMultipleWordsWithLeadingNumber" $ do
            parseCook "Top with @1000 island dressing{ }" `shouldBe` (Right $ Recipe [][[Text "Top with ",Ingredient "1000 island dressing" "\"some\"" ""]])

        it "IngredientWithEmoji" $ do
            parseCook "Add some @🧂" `shouldBe` (Right $ Recipe [][[Text "Add some ",Ingredient "\129474" "\"some\"" ""]])

        it "IngridentExplicitUnits" $ do
            parseCook "@chilli{3%items}" `shouldBe` (Right $ Recipe [][[Ingredient "chilli" "3" "items"]])

        it "IngridentExplicitUnitsWithSpaces" $ do
            parseCook "@chilli{ 3 % items }" `shouldBe` (Right $ Recipe [][[Ingredient "chilli" "3" "items"]])

        it "IngridentImplicitUnits" $ do
            parseCook "@chilli{3}" `shouldBe` (Right $ Recipe [][[Ingredient "chilli" "3" ""]])

        it "IngridentNoUnits" $ do
            parseCook "@chilli" `shouldBe` (Right $ Recipe [][[Ingredient "chilli" "\"some\"" ""]])

        it "IngridentNoUnitsNotOnlyString" $ do
            parseCook "@5peppers" `shouldBe` (Right $ Recipe [][[Ingredient "5peppers" "\"some\"" ""]])

        it "IngridentWithNumbers" $ do
            parseCook "@tipo 00 flour{250%g}" `shouldBe` (Right $ Recipe [][[Ingredient "tipo 00 flour" "250" "g"]])

        it "IngridentWithoutStopper" $ do
            parseCook "@chilli cut into pieces" `shouldBe` (Right $ Recipe [][[Ingredient "chilli" "\"some\"" "",Text " cut into pieces"]])

        it "Metadata" $ do
            parseCook ">> sourced: babooshka" `shouldBe` (Right $ Recipe [("sourced","babooshka")][[]])

        it "MetadataBreak" $ do
            parseCook "hello >> sourced: babooshka" `shouldBe` (Right $ Recipe [][[Text "hello >> sourced: babooshka"]])

        it "MetadataMultiwordKey" $ do
            parseCook ">> cooking time: 30 mins" `shouldBe` (Right $ Recipe [("cooking time","30 mins")][[]])

        it "MetadataMultiwordKeyWithSpaces" $ do
            parseCook ">>cooking time    :30 mins" `shouldBe` (Right $ Recipe [("cooking time","30 mins")][[]])

        it "MultiLineDirections" $ do
            parseCook "Add a bit of chilli\n\n\nAdd a bit of hummus" `shouldBe` (Right $ Recipe [][[Text "Add a bit of chilli"],[Text "Add a bit of hummus"]])

        it "MultipleLines" $ do
            parseCook ">> Prep Time: 15 minutes\n>> Cook Time: 30 minutes" `shouldBe` (Right $ Recipe [("Prep Time","15 minutes"),("Cook Time","30 minutes")][[]])

        it "MultiWordIngrident" $ do
            parseCook "@hot chilli{3}" `shouldBe` (Right $ Recipe [][[Ingredient "hot chilli" "3" ""]])

        it "MultiWordIngridentNoAmount" $ do
            parseCook "@hot chilli{}" `shouldBe` (Right $ Recipe [][[Ingredient "hot chilli" "\"some\"" ""]])

        it "MutipleIngridentsWithoutStopper" $ do
            parseCook "@chilli cut into pieces and @garlic" `shouldBe` (Right $ Recipe [][[Ingredient "chilli" "\"some\"" "",Text " cut into pieces and ",Ingredient "garlic" "\"some\"" ""]])

        it "QuantityAsText" $ do
            parseCook "@thyme{few%springs}" `shouldBe` (Right $ Recipe [][[Ingredient "thyme" "few" "springs"]])

        it "QuantityDigitalString" $ do
            parseCook "@water{7 k }" `shouldBe` (Right $ Recipe [][[Ingredient "water" "7 k" ""]])

        it "Servings" $ do
            parseCook ">> servings: 1|2|3" `shouldBe` (Right $ Recipe [("servings","1|2|3")][[]])

        it "SlashInText" $ do
            parseCook "Preheat the oven to 200℃/Fan 180°C." `shouldBe` (Right $ Recipe [][[Text "Preheat the oven to 200\8451/Fan 180\176C."]])

        it "TimerDecimal" $ do
            parseCook "Fry for ~{1.5%minutes}" `shouldBe` (Right $ Recipe [][[Text "Fry for ",Timer "" "1.5" "minutes"]])

        it "TimerFractional" $ do
            parseCook "Fry for ~{1/2%hour}" `shouldBe` (Right $ Recipe [][[Text "Fry for ",Timer "" "0.5" "hour"]])

        it "TimerInteger" $ do
            parseCook "Fry for ~{10%minutes}" `shouldBe` (Right $ Recipe [][[Text "Fry for ",Timer "" "10" "minutes"]])

        it "TimerWithName" $ do
            parseCook "Fry for ~potato{42%minutes}" `shouldBe` (Right $ Recipe [][[Text "Fry for ",Timer "potato" "42" "minutes"]])

