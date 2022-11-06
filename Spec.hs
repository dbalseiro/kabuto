{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Kabuto.Parser
import Kabuto.Parser.Types
import Control.Exception (try)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $
    it "works on fixture" $ do
      parsed <- try $ parseModuleIO "fixtures/sample.schema"
      case parsed of
        Left (ParseException err) -> putStrLn err >> fail "Parse Exception"
      parsed `shouldBe` expected


expected :: [TypeDescriptor]
expected = 
  [ TypeDescriptor
    ( TypeName {unTypeName = "Student"} )
    ( ProductTypeDef
      [ FieldDef {fieldName = FieldName {unFieldName = "id"}, fieldType = ScalarType (TypeName {unTypeName = "StudentId"})}
      , FieldDef {fieldName = FieldName {unFieldName = "name"}, fieldType = ScalarType (TypeName {unTypeName = "StudentName"})}
      , FieldDef {fieldName = FieldName {unFieldName = "age"}, fieldType = ScalarType (TypeName {unTypeName = "StudentAge"})}
      , FieldDef {fieldName = FieldName {unFieldName = "hasDebt"}, fieldType = BaseType BoolType}
      , FieldDef {fieldName = FieldName {unFieldName = "classes"}, fieldType = ListType (ScalarType (TypeName {unTypeName = "Class"}))}
      ]
    )
  , TypeDescriptor
    ( TypeName {unTypeName = "Class"} )
    ( SumTypeDef
      [ ScalarType (TypeName {unTypeName = "Lecture"})
      , ScalarType (TypeName {unTypeName = "Lab"})
      ]
    )
  , TypeDescriptor
    ( TypeName {unTypeName = "Lecture"} )
    ( ProductTypeDef
      [ FieldDef {fieldName = FieldName {unFieldName = "professorName"}, fieldType = ScalarType (TypeName {unTypeName = "ProfessorName"})}
      , FieldDef {fieldName = FieldName {unFieldName = "topic"}, fieldType = BaseType StringType}
      ]
    )
  , TypeDescriptor
    ( TypeName {unTypeName = "Lab"} )
    ( ProductTypeDef
      [ FieldDef {fieldName = FieldName {unFieldName = "professorName"}, fieldType = ScalarType (TypeName {unTypeName = "ProfessorName"})}
      , FieldDef {fieldName = FieldName {unFieldName = "hours"}, fieldType = BaseType IntType}
      ]
    )
  , TypeDescriptor
    ( TypeName {unTypeName = "ProfessorName"} )
    ( NamedTypeDef (ScalarType (TypeName {unTypeName = "Name"}) ))
  ,TypeDescriptor
    ( TypeName {unTypeName = "StudentName"} )
    ( NamedTypeDef (ScalarType (TypeName {unTypeName = "Name"}) ))
  ,TypeDescriptor
    ( TypeName {unTypeName = "Name"})
    ( ProductTypeDef
      [ FieldDef {fieldName = FieldName {unFieldName = "first"}, fieldType = BaseType StringType }
      , FieldDef {fieldName = FieldName {unFieldName = "last"}, fieldType = BaseType StringType }
      ]
    )
  , TypeDescriptor
    ( TypeName {unTypeName = "StudentAge"})
    ( NamedTypeDef (BaseType IntType))
  ]
