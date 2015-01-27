module Handlers.Api where

import Imports

import Session.User
import Sql.Queries
import Sql.User

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

getClassesR :: Handler Value
getClassesR = jsonResult $ do
    uid <- requireUser
    runUserSQL uid $ getClasses

postClassesR :: Handler Value
postClassesR = jsonResult $ do
    uid <- requireUser
    cl <- runInputPost classForm
    _ <- runUserSQL uid $ createClass cl
    return ()

getClassR :: Text -> Handler Value
getClassR className = jsonResult $ do
    uid <- requireUser
    validateName className
    runUserSQL uid $ getClass className
            `orElseM` fail "Class does not exist."

deleteClassR :: Text -> Handler Value
deleteClassR className = jsonResult $ do
    uid <- requireUser
    validateName className
    runUserSQL uid $ do
        classId <- getClassId className
                   `orElseM` fail "Class does not exist."
        deleteDecl classId

getFieldsR :: Text -> Handler Value
getFieldsR className = jsonResult $ do
    uid <- requireUser
    validateName className
    runUserSQL uid $ do
        cid <- getTypeId className
               `orElseM` fail "Class does not exist."
        getFields cid

postFieldsR :: Text -> Handler Value
postFieldsR className = jsonResult $ do
    uid <- requireUser
    validateName className
    field <- runInputPost fieldForm
    _ <- runUserSQL uid $ do
        classId <- getClassId className
                   `orElseM` fail "Class does not exist."
        createField classId field
    return ()

getFieldR :: Text -> Text -> Handler Value
getFieldR className fieldIdent = jsonResult $ do
    uid <- requireUser
    validateName className
    validateIdent fieldIdent
    runUserSQL uid $ do
        classId <- getClassId className
                   `orElseM` fail "Class does not exist."
        getField classId fieldIdent
            `orElseM` fail "Field does not exist."

deleteFieldR :: Text -> Text -> Handler Value
deleteFieldR className fieldIdent = jsonResult $ do
    uid <- requireUser
    validateName className
    validateIdent fieldIdent
    runUserSQL uid $ do
        classId <- getClassId className
                   `orElseM` fail "Class does not exist."
        fieldId <- getFieldId classId fieldIdent
            `orElseM` fail "Field does not exist."
        deleteDecl fieldId

getMethodsR :: Text -> Handler Value
getMethodsR className = jsonResult $ do
    uid <- requireUser
    validateName className
    methods <- runUserSQL uid $ do
        classId <- getClassId className
                   `orElseM` fail "Class does not exist."
        getMethods classId
    return $ map methodWithArgs methods

postMethodsR :: Text -> Handler Value
postMethodsR className = jsonResult $ do
    uid <- requireUser
    validateName className
    (method, args) <- runInputPost methodForm
    mId <- runUserSQL uid $ do
        classId <- getClassId className
                   `orElseM` fail "Class does not exist."
        createMethod classId method args
    let method' = method { methodId = mId }
    return $ methodWithArgs (method', args)

getMethodR :: MethodId -> Handler Value
getMethodR methodId = jsonResult $ do
    uid <- requireUser
    method <- runUserSQL uid $ getMethod methodId
              `orElseM` fail "Method does not exist."
    return $ methodWithArgs method

deleteMethodR :: MethodId -> Handler Value
deleteMethodR methodId = jsonResult $ do
    uid <- requireUser
    runUserSQL uid $ do
        getMethod methodId `orElseM` fail "Method does not exist."
        deleteDecl methodId
