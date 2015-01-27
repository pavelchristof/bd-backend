{-# LANGUAGE UndecidableInstances #-}
module Types where

import ClassyPrelude.Yesod
import Data.Either
import Database.Persist.Sql
import Language.Java.Parser hiding (fieldDecl)
import Text.Parsec (eof)

import Foundation
import Sql

-- Ids.

type UserId = Int
type DeclId = Int
type TypeId = Int
type ValueId = Int
type ClassId = Int
type FieldId = Int

-- Declarations.

newtype DBDecl = DBDecl {
    declFile :: Maybe Text
}

instance SqlRow DBDecl where
    type RawSqlType DBDecl = Single (Maybe Text)
    fromRawSql = DBDecl . unSingle

instance ToJSON DBDecl where
    toJSON (DBDecl f) = object
        [ "file" .= f ]

-- Types.

data DBType = DBType {
    typeDecl :: DBDecl,
    typeName :: Text
}

instance SqlRow DBType where
    type RawSqlType DBType = (RawSqlType DBDecl, Single Text)
    fromRawSql (decl, Single n) = DBType (fromRawSql decl) n

instance ToJSON DBType where
    toJSON (DBType decl n) = object
        [ "decl" .= toJSON decl
        , "name" .= n ]

-- Values.

data DBValue = DBValue {
    valueDecl :: DBDecl,
    valueIdent :: Text
}

instance SqlRow DBValue where
    type RawSqlType DBValue = (RawSqlType DBDecl, Single Text)
    fromRawSql (decl, Single i) = DBValue (fromRawSql decl) i

instance ToJSON DBValue where
    toJSON (DBValue decl identi) = object
        [ "decl" .= toJSON decl
        , "ident" .= identi ]

-- Classes.

data DBClass = DBClass {
    classType :: DBType,
    classIsStruct :: Bool
}

instance SqlRow DBClass where
    type RawSqlType DBClass = (RawSqlType DBType, Single Bool)
    fromRawSql (ty, Single b) = DBClass (fromRawSql ty) b

instance ToJSON DBClass where
    toJSON (DBClass ty b) = object
         [ "type" .= toJSON ty
         , "isStruct" .= b ]

-- Fields.

data DBField = DBField {
    fieldValue :: DBValue,
    fieldStatic :: Bool,
    fieldTypeName :: Text
}

instance SqlRow DBField where
    type RawSqlType DBField = (RawSqlType DBValue, Single Bool, Single Text)
    fromRawSql (val, Single st, Single ty) = DBField (fromRawSql val) st ty

instance ToJSON DBField where
    toJSON f = object
        [ "value" .= toJSON (fieldValue f)
        , "static" .= fieldStatic f
        , "typeName" .= fieldTypeName f ]

-- Methods.

data DBMethod = DBMethod {
    methodValue :: DBValue,
    methodClass :: Text,
    methodStatic :: Bool,
    methodReturnType :: Text
}

instance SqlRow DBMethod where
    type RawSqlType DBMethod = (RawSqlType DBValue, Single Text, Single Bool, Single Text)
    fromRawSql (val, Single cid, Single s, Single rt) = DBMethod (fromRawSql val) cid s rt

-- Method args.

newtype DBMethodArgs = DBMethodArgs { getMethodArgs :: [Text] }

instance ToJSON DBMethodArgs where
    toJSON (DBMethodArgs xs) = toJSON xs

methodWithArgs :: DBMethod -> DBMethodArgs -> Value
methodWithArgs m a = object
    [ "value" .= toJSON (methodValue m)
    , "class" .= methodClass m
    , "static" .= toJSON (methodStatic m)
    , "returnType" .= methodReturnType m
    , "argTypes" .= toJSON a
    ]

-- Validation.

parse what = parser (what <* eof) . unpack

validate check msg x =
    if check x
       then return ()
       else fail msg

isIdentOk :: Text -> Bool
isIdentOk = isRight . parse ident

isNameOk :: Text -> Bool
isNameOk = isRight . parse name

validateIdent :: Text -> Handler ()
validateIdent = validate isIdentOk "Invalid identifier."

validateName :: Text -> Handler ()
validateName = validate isNameOk "Invalid name."
