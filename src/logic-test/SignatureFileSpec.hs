module SignatureFileSpec (spec) where

import Base.Prelude
import Logic.SignatureFile
import Test.Hspec

spec :: Spec
spec = do
  describe "serialize and tryParse roundtrip" do
    it "roundtrips a signature with parameters and result" do
      let sig =
            Signature
              { parameters =
                  [ ("format", FieldSig {typeName = "uuid", notNull = False}),
                    ("name", FieldSig {typeName = "text", notNull = True})
                  ],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityZeroOrOne,
                        columns =
                          [ ("id", FieldSig {typeName = "uuid", notNull = True}),
                            ("name", FieldSig {typeName = "text", notNull = True}),
                            ("released", FieldSig {typeName = "date", notNull = False})
                          ]
                      }
              }
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips a signature with empty parameters" do
      let sig =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
      tryParse (serialize sig) `shouldBe` Right sig

    it "roundtrips a signature with no result" do
      let sig =
            Signature
              { parameters =
                  [ ("id", FieldSig {typeName = "int8", notNull = True})
                  ],
                result = Nothing
              }
      tryParse (serialize sig) `shouldBe` Right sig

  describe "serialize" do
    it "produces expected YAML format" do
      let sig =
            Signature
              { parameters =
                  [ ("format", FieldSig {typeName = "uuid", notNull = False})
                  ],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityZeroOrOne,
                        columns =
                          [ ("id", FieldSig {typeName = "uuid", notNull = True})
                          ]
                      }
              }
          expected =
            "parameters:\n\
            \  format:\n\
            \    type: uuid\n\
            \    not_null: false\n\
            \result:\n\
            \  cardinality: zero_or_one\n\
            \  columns:\n\
            \    id:\n\
            \      type: uuid\n\
            \      not_null: true\n"
      serialize sig `shouldBe` expected

    it "serializes one-dimensional array types using nested syntax" do
      let sig =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ( "tracks",
                              ArrayFieldSig
                                { typeName = "track_info[]",
                                  notNull = False,
                                  elementNotNull = False
                                }
                            )
                          ]
                      }
              }
          expected =
            "parameters: {}\n\
            \result:\n\
            \  cardinality: many\n\
            \  columns:\n\
            \    tracks:\n\
            \      type:\n\
            \        array:\n\
            \          element:\n\
            \            name: track_info\n\
            \            not_null: false\n\
            \      not_null: false\n"
      serialize sig `shouldBe` expected

  describe "tryParse" do
    it "parses all cardinality values" do
      let mkSig card =
            "parameters: {}\nresult:\n  cardinality: "
              <> card
              <> "\n  columns:\n    id:\n      type: int4\n      not_null: true\n"
      fmap (.result) (tryParse (mkSig "zero_or_one"))
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityZeroOrOne,
                  columns = [("id", FieldSig {typeName = "int4", notNull = True})]
                }
          )
      fmap (.result) (tryParse (mkSig "one"))
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityOne,
                  columns = [("id", FieldSig {typeName = "int4", notNull = True})]
                }
          )
      fmap (.result) (tryParse (mkSig "many"))
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityMany,
                  columns = [("id", FieldSig {typeName = "int4", notNull = True})]
                }
          )

    it "rejects invalid cardinality" do
      let yaml =
            "parameters: {}\nresult:\n  cardinality: invalid\n  columns:\n    id:\n      type: int4\n      not_null: true\n"
      tryParse yaml `shouldSatisfy` isLeft

    it "parses array types with nested element nullability" do
      let yaml =
            "parameters: {}\n\
            \result:\n\
            \  cardinality: many\n\
            \  columns:\n\
            \    tracks:\n\
            \      type:\n\
            \        array:\n\
            \          element:\n\
            \            name: track_info\n\
            \            not_null: false\n\
            \      not_null: false\n"
      fmap (.result) (tryParse yaml)
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityMany,
                  columns =
                    [ ( "tracks",
                        ArrayFieldSig
                          { typeName = "track_info[]",
                            notNull = False,
                            elementNotNull = False
                          }
                      )
                    ]
                }
          )

    it "parses legacy array syntax" do
      let yaml =
            "parameters: {}\n\
            \result:\n\
            \  cardinality: many\n\
            \  columns:\n\
            \    tracks:\n\
            \      type: track_info[]\n\
            \      not_null: false\n"
      fmap (.result) (tryParse yaml)
        `shouldBe` Right
          ( Just
              ResultSig
                { cardinality = CardinalityMany,
                  columns =
                    [ ( "tracks",
                        FieldSig
                          { typeName = "track_info[]",
                            notNull = False
                          }
                      )
                    ]
                }
          )

  describe "validateAndMerge" do
    it "accepts matching signatures" do
      let sig =
            Signature
              { parameters =
                  [ ("x", FieldSig {typeName = "int4", notNull = False})
                  ],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
      validateAndMerge sig sig `shouldBe` Right sig

    it "allows parameter not_null to be made stricter (false -> true)" do
      let inferred =
            Signature
              { parameters =
                  [ ("x", FieldSig {typeName = "int4", notNull = False})
                  ],
                result = Nothing
              }
          file =
            Signature
              { parameters =
                  [ ("x", FieldSig {typeName = "int4", notNull = True})
                  ],
                result = Nothing
              }
      validateAndMerge inferred file `shouldBe` Right file

    it "rejects parameter not_null relaxation (true -> false)" do
      let inferred =
            Signature
              { parameters =
                  [ ("x", FieldSig {typeName = "int4", notNull = True})
                  ],
                result = Nothing
              }
          file =
            Signature
              { parameters =
                  [ ("x", FieldSig {typeName = "int4", notNull = False})
                  ],
                result = Nothing
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft

    it "rejects result column not_null relaxation (true -> false)" do
      let inferred =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
          file =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = False})
                          ]
                      }
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft

    it "allows result column not_null to be made stricter (false -> true)" do
      let inferred =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = False})
                          ]
                      }
              }
          file =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
      validateAndMerge inferred file `shouldBe` Right file

    it "allows cardinality to be changed freely" do
      let inferred =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
          file =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityOne,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
          expected =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityOne,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
      validateAndMerge inferred file `shouldBe` Right expected

    it "keeps inferred array element nullability when file uses legacy array syntax" do
      let inferred =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ( "tracks",
                              ArrayFieldSig
                                { typeName = "track_info[]",
                                  notNull = False,
                                  elementNotNull = True
                                }
                            )
                          ]
                      }
              }
          file =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ( "tracks",
                              FieldSig
                                { typeName = "track_info[]",
                                  notNull = False
                                }
                            )
                          ]
                      }
              }
          expected =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ( "tracks",
                              ArrayFieldSig
                                { typeName = "track_info[]",
                                  notNull = False,
                                  elementNotNull = True
                                }
                            )
                          ]
                      }
              }
      validateAndMerge inferred file `shouldBe` Right expected

    it "rejects type mismatch in parameters" do
      let inferred =
            Signature
              { parameters =
                  [ ("x", FieldSig {typeName = "int4", notNull = False})
                  ],
                result = Nothing
              }
          file =
            Signature
              { parameters =
                  [ ("x", FieldSig {typeName = "int8", notNull = False})
                  ],
                result = Nothing
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft

    it "rejects type mismatch in result columns" do
      let inferred =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
          file =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", FieldSig {typeName = "uuid", notNull = True})
                          ]
                      }
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft

    it "rejects parameter name mismatch" do
      let inferred =
            Signature
              { parameters =
                  [ ("x", FieldSig {typeName = "int4", notNull = False})
                  ],
                result = Nothing
              }
          file =
            Signature
              { parameters =
                  [ ("y", FieldSig {typeName = "int4", notNull = False})
                  ],
                result = Nothing
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft

    it "rejects result section mismatch (present vs absent)" do
      let inferred =
            Signature
              { parameters = [],
                result =
                  Just
                    ResultSig
                      { cardinality = CardinalityMany,
                        columns =
                          [ ("id", FieldSig {typeName = "int4", notNull = True})
                          ]
                      }
              }
          file =
            Signature
              { parameters = [],
                result = Nothing
              }
      validateAndMerge inferred file `shouldSatisfy` isLeft
