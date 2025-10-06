{-# LANGUAGE OverloadedStrings #-}

module JsonSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Json
import Test.Hspec

spec :: Spec
spec = do
  describe "empty root elements" $ do
    it "should return an empty object" $ do
      let json = Json.parse "{}"
      json `shouldSatisfy` Maybe.isJust
      Maybe.fromJust json `shouldBe` Json.Object Map.empty

    it "should return an empty array" $ do
      let json = Json.parse "[]"
      json `shouldSatisfy` Maybe.isJust
      Maybe.fromJust json `shouldBe` Json.Array []

  describe "parse error" $ do
    it "should return nothing if parsing fails" $ do
      let notJson = Json.parse "{ not valid ] }"
      notJson `shouldBe` Nothing

  describe "values" $ do
    it "should parse strings" $ do
      Json.parse "\"some string\"" `shouldBe` Just (Json.String "some string")

    it "should parse booleans" $ do
      Json.parse "true" `shouldBe` Just (Json.Boolean True)
      Json.parse "false" `shouldBe` Just (Json.Boolean False)

    it "should parse numbers" $ do
      Json.parse "1234567" `shouldBe` Just (Json.Number 1234567)
      Json.parse "-42" `shouldBe` Just (Json.Number $ -42)
      Json.parse "3.1415" `shouldBe` Just (Json.Number 3.1415)
      Json.parse "-0.0001" `shouldBe` Just (Json.Number $ -0.0001)

    it "should parse null" $ do
      Json.parse "null" `shouldBe` Just Json.Null

  describe "object" $ do
    it "should return the object and its values" $ do
      let json =
            Json.parse "[{ \"value\": 42, \"string\": \"a string\", \"flag\": true, \"values\": [1,2,3], \"nil\": null, \"object\": {}}]"
      json `shouldSatisfy` Maybe.isJust
      Maybe.fromJust json
        `shouldBe` Json.Array
          [ Json.Object $
              Map.fromList
                [ ("flag", Json.Boolean True),
                  ("nil", Json.Null),
                  ("object", Json.Object Map.empty),
                  ("string", Json.String "a string"),
                  ("value", Json.Number 42),
                  ("values", Json.Array [Json.Number 1, Json.Number 2, Json.Number 3])
                ]
          ]

  describe "array" $ do
    it "should allow arrays of items with different types" $ do
      let json = Json.parse "[\"foo\", 17, null]"
      json `shouldSatisfy` Maybe.isJust
      Maybe.fromJust json `shouldBe` Json.Array [Json.String "foo", Json.Number 17, Json.Null]

  describe "whitespaces" $ do
    it "should skip all kinds of whitespaces" $ do
      let json = Json.parse "   \n  \n { \t \n\"foo\" : \t42 \n , \"bar\" \n : \t\t false \t } \n\t "
      json `shouldSatisfy` Maybe.isJust
      Maybe.fromJust json
        `shouldBe` Json.Object
          ( Map.fromList
              [ ("foo", Json.Number 42),
                ("bar", Json.Boolean False)
              ]
          )

  describe "identifiers" $ do
    it "should parse any string as identifier" $ do
      Json.parse "{\"foo_bar\": null}" `shouldBe` Just (Json.Object $ Map.fromList [("foo_bar", Json.Null)])
      Json.parse "{\"foo bar\": null}" `shouldBe` Just (Json.Object $ Map.fromList [("foo bar", Json.Null)])
      Json.parse "{\"F🌸⭐️ バー\": null}" `shouldBe` Just (Json.Object $ Map.fromList [("F🌸⭐️ バー", Json.Null)])
