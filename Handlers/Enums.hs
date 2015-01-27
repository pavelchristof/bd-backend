module Handlers.Enums where

import Forms
import Imports
import Session.User
import Sql.Common
import Sql.Enums
import Sql.User

getEnumsR :: Handler Value
getEnumsR = jsonResult $ do
    uid <- requireUser
    runUserSQL uid getEnums

postEnumsR :: Handler Value
postEnumsR = jsonResult $ do
    uid <- requireUser
    e <- runInputPost enumForm
    _ <- runUserSQL uid $ createEnum e
    return ()

getEnumR :: Text -> Handler Value
getEnumR name = jsonResult $ do
    uid <- requireUser
    validateName name
    runUserSQL uid $ getEnum name
                     `orElseM` fail "The enum does not exist."

deleteEnumR :: Text -> Handler Value
deleteEnumR name = jsonResult $ do
    uid <- requireUser
    validateName name
    runUserSQL uid $ do
        enumId <- getEnumId name
                  `orElseM` fail "The enum does not exist."
        deleteDecl enumId
