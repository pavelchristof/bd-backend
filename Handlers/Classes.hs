module Handlers.Classes where

import Forms
import Imports
import Session.User
import Sql.Classes
import Sql.Common
import Sql.User

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
        _ <- getMethod methodId `orElseM` fail "Method does not exist."
        deleteDecl methodId

getReadersR :: Text -> Text -> Handler Value
getReadersR className fieldName = jsonResult $ do
    uid <- requireUser
    res <- runUserSQL uid $ do
        classId <- getClassId className `orElseM` fail "Class does not exist."
        fieldId <- getFieldId classId fieldName `orElseM` fail "Field does not exist."
        getReaders fieldId
    return $ map (\(a, b) -> a <> "." <> b) res

getWritersR :: Text -> Text -> Handler Value
getWritersR className fieldName = jsonResult $ do
    uid <- requireUser
    res <- runUserSQL uid $ do
        classId <- getClassId className `orElseM` fail "Class does not exist."
        fieldId <- getFieldId classId fieldName `orElseM` fail "Field does not exist."
        getWriters fieldId
    return $ map (\(a, b) -> a <> "." <> b) res

getCallersR :: MethodId -> Handler Value
getCallersR methodId = jsonResult $ do
    uid <- requireUser
    res <- runUserSQL uid $ getCallers methodId
    return $ map (\(a, b) -> a <> "." <> b) res
