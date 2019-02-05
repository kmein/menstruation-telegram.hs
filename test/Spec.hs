{-# LANGUAGE QuasiQuotes, OverloadedStrings, OverloadedLists #-}

import Data.Aeson
import Data.Aeson.QQ
import Data.Time
import Test.Hspec
import Test.QuickCheck (property)

import Menstruation.Mensa
import Menstruation.Menu
import Menstruation.Settings

import Instances

main =
  hspec $ do
    describe "Menstruation.Menu.Meal" $
      it "parses JSON correctly" $ do
        fromJSON
          [aesonQQ|{ name: "Schlonze", color: "green", tags: ["vegetarian", "vegetarian", "sustainable fishing"], price: { student: 100, employee: 150, guest: 200 }, allergens: ["34c"] }|] `shouldBe`
          Success
            Meal
              { mealName = "Schlonze"
              , mealColor = Green
              , mealTags = [Vegetarian, SustainableFishing]
              , mealPrice = Just Price {student = 100, employee = 150, guest = 200}
              , mealAllergens = [Allergen {allergenNumber = 34, allergenTag = Just "c"}]
              }
        fromJSON
          [aesonQQ|{ name: "Gratis-Schlonze", color: "green", tags: [], price: null, allergens: [] }|] `shouldBe`
          Success
            Meal
              { mealName = "Gratis-Schlonze"
              , mealColor = Green
              , mealTags = []
              , mealPrice = Nothing
              , mealAllergens = []
              }
    describe "Menstruation.Mensa.Mensa" $
      it "parses JSON correctly" $
      fromJSON [aesonQQ|{name: "Dönermann", address: "Überall, Berlin", code: 40 }|] `shouldBe`
      Success Mensa {mensaName = "Dönermann", mensaAddress = "Überall, Berlin", mensaCode = 40}
    describe "Menstruation.Settings.Date" $ do
      it "parses 'tomorrow'" $ extractDate "tomorrow" `shouldBe` Just Tomorrow
      it "parses YYYY-MM-DD" $
        extractDate "2018-10-22" `shouldBe` Just (Selected (fromGregorian 2018 10 22))
      it "ignores missing zeroes" $ do
        extractDate "2019-1-1" `shouldBe` extractDate "2019-01-01"
        extractDate "1-2-3" `shouldBe` extractDate "0001-02-03"
      it "does not parse today" $ extractDate "today" `shouldBe` Nothing
      it "does not parse DD.MM.YYYY" $ extractDate "22.10.1999" `shouldBe` Nothing
    describe "Menstruation.Settings.Filter" $ do
      it "parses correctly" $ extractFilter "3,5" `shouldBe` mempty {maximumPrice = Just 350}
      it "ignores missing zeroes" $ extractFilter "4,00" `shouldBe` extractFilter "4"
      it "keeps all when using empty filter" $ property $ \m -> applyFilter mempty m == m
