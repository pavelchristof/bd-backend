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

nameListField :: Field Handler [Text]
nameListField = Field
    { fieldParse = \txts _ -> do
        mapM_ validateName txts
        return $ Right $ Just txts
    , fieldView = \_ _ _ _ _ -> return ()
    , fieldEnctype = UrlEncoded
    }

-- Forms.

declForm :: FormInput Handler DBDecl
declForm = DBDecl
    <$> iopt textField "file"

valueForm :: FormInput Handler DBValue
valueForm = DBValue
    <$> declForm
    <*> ireq identField "ident"

typeForm :: FormInput Handler DBType
typeForm = DBType
    <$> declForm
    <*> ireq nameField "name"

classForm :: FormInput Handler DBClass
classForm = DBClass
    <$> typeForm
    <*> ireq boolField "isStruct"

fieldForm :: FormInput Handler DBField
fieldForm = DBField
    <$> valueForm
    <*> ireq boolField "static"
    <*> ireq nameField "type"

methodForm :: FormInput Handler (DBMethod, DBMethodArgs)
methodForm = (,) <$> body <*> args
  where
    body = DBMethod
        <$> pure 0
        <*> valueForm
        <*> ireq boolField "static"
        <*> ireq nameField "returnType"
    args = DBMethodArgs
        <$> ireq nameListField "argTypes"

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

deleteClassR :: Text -> Handler Value
deleteClassR className = jsonResult $ do
    uid <- requireUser
    validateName className
    runDB $ do
        classId <- getClassId uid className
                   `orElseM` fail "Class does not exist."
        deleteDecl uid classId

getFieldsR :: Text -> Handler Value
getFieldsR className = jsonResult $ do
    uid <- requireUser
    validateName className
    runDB $ do
        cid <- getTypeId uid className
               `orElseM` fail "Class does not exist."
        getFields uid cid

postFieldsR :: Text -> Handler Value
postFieldsR className = jsonResult $ do
    uid <- requireUser
    validateName className
    field <- runInputPost fieldForm
    _ <- runDB $ do
        classId <- getClassId uid className
                   `orElseM` fail "Class does not exist."
        createField uid classId field
    return ()

getFieldR :: Text -> Text -> Handler Value
getFieldR className fieldIdent = jsonResult $ do
    uid <- requireUser
    validateName className
    validateIdent fieldIdent
    runDB $ do
        classId <- getClassId uid className
                   `orElseM` fail "Class does not exist."
        getField uid classId fieldIdent
            `orElseM` fail "Field does not exist."

deleteFieldR :: Text -> Text -> Handler Value
deleteFieldR className fieldIdent = jsonResult $ do
    uid <- requireUser
    validateName className
    validateIdent fieldIdent
    runDB $ do
        classId <- getClassId uid className
                   `orElseM` fail "Class does not exist."
        fieldId <- getFieldId uid classId fieldIdent
            `orElseM` fail "Field does not exist."
        deleteDecl uid fieldId

getMethodsR :: Text -> Handler Value
getMethodsR className = jsonResult $ do
    uid <- requireUser
    validateName className
    methods <- runDB $ do
        classId <- getClassId uid className
                   `orElseM` fail "Class does not exist."
        getMethods uid classId
    return $ map methodWithArgs methods

postMethodsR :: Text -> Handler Value
postMethodsR className = jsonResult $ do
    uid <- requireUser
    validateName className
    (method, args) <- runInputPost methodForm
    mId <- runDB $ do
        classId <- getClassId uid className
                   `orElseM` fail "Class does not exist."
        createMethod uid classId method args
    let method' = method { methodId = mId }
    return $ methodWithArgs (method', args)

getMethodR :: MethodId -> Handler Value
getMethodR methodId = jsonResult $ do
    uid <- requireUser
    method <- runDB $ getMethod uid methodId
              `orElseM` fail "Method does not exist."
    return $ methodWithArgs method

deleteMethodR :: MethodId -> Handler Value
deleteMethodR methodId = jsonResult $ do
    uid <- requireUser
    runDB $ do
        getMethod uid methodId `orElseM` fail "Method does not exist."
        deleteDecl uid methodId
