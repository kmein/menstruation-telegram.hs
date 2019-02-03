{-# LANGUAGE QuasiQuotes, OverloadedStrings, OverloadedLists #-}

import Data.Aeson
import Data.Aeson.QQ
import Test.Hspec
import Test.QuickCheck ()

import Menstruation.Mensa
import Menstruation.Menu

main =
  hspec $ do
    describe "Meal" $
      it "parses JSON correctly" $ do
        fromJSON
          [aesonQQ|{ name: "Schlonze", color: "green", tags: ["vegetarian", "vegetarian", "sustainable fishing"], price: { student: 100, employee: 150, guest: 200 }, allergens: [] }|] `shouldBe`
          Success
            Meal
              { mealName = "Schlonze"
              , mealColor = Green
              , mealTags = [Vegetarian, SustainableFishing]
              , mealPrice = Just Price {student = 100, employee = 150, guest = 200}
              , mealAllergens = []
              }
        fromJSON
          [aesonQQ|{ name: "Gratis Schlonze", color: "green", tags: [], price: null, allergens: [] }|] `shouldBe`
          Success
            Meal
              { mealName = "Gratis Schlonze"
              , mealColor = Green
              , mealTags = []
              , mealPrice = Nothing
              , mealAllergens = []
              }
    describe "Mensa" $
      it "parses JSON correctly" $
      fromJSON [aesonQQ|{name: "Dönermann", address: "Überall, Berlin", code: 40 }|] `shouldBe`
      Success Mensa {mensaName = "Dönermann", mensaAddress = "Überall, Berlin", mensaCode = 40}
