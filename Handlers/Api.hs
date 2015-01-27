module Handlers.Api where

import Imports

import Session.User
import Sql.Queries

-- Validation.

identField :: Field Handler Text
identField = checkBool
    isIdentOk
    ("Invalid identifier." :: Text)
    textField

nameField :: Field Handler Text
nameField = checkBool
    isNameOk
    ("Invalid name." :: Text)
    textField

-- Forms.

declForm :: FormInput Handler DBDecl
declForm = DBDecl <$> iopt textField "file"

typeForm :: FormInput Handler DBType
typeForm = DBType
    <$> declForm
    <*> ireq nameField "name"

classForm :: FormInput Handler DBClass
classForm = DBClass
    <$> typeForm
    <*> ireq boolField "isStruct"

-- Handlers.

postClassesR :: Handler Value
postClassesR = jsonResult $ do
    uid <- requireUser
    cl <- runInputPost classForm
    _ <- runDB $ createClass uid cl
    return ()

getClassR :: Text -> Handler Value
getClassR className = jsonResult $ do
    uid <- requireUser
    validateName className
    runDB $ getClass uid className
            `orElseM` fail "Class does not exist."

getFieldsR :: Text -> Handler Value
getFieldsR className = jsonResult $ do
    uid <- requireUser
    validateName className
    fields <- runDB $ do
        cid <- getTypeId uid className
               `orElseM` fail "Class does not exist."
        getFields uid cid
    return fields

postFieldsR :: Text -> Handler Value
postFieldsR className = jsonResult $ do
    uid <- requireUser
    --field <- runInputPost fieldForm
    return ()
  where
    -- fieldForm = DBField
    --     <$> ireq identField "fieldIdent"
    --     <*> ireq boolField "fieldStatic"
    --     <*> ireq nameField "fieldTypeName"
