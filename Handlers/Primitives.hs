module Handlers.Primitives where

import Forms
import Imports
import Session.User
import Sql.Common
import Sql.Primitives
import Sql.User

getPrimitivesR :: Handler Value
getPrimitivesR = jsonResult $ do
    uid <- requireUser
    runUserSQL uid $ getPrimitives

postPrimitivesR :: Handler Value
postPrimitivesR = jsonResult $ do
    uid <- requireUser
    ty <- runInputPost typeForm
    _ <- runUserSQL uid $ createPrimitive ty
    return ()

getPrimitiveR :: Text -> Handler Value
getPrimitiveR primName = jsonResult $ do
    uid <- requireUser
    validateName primName
    runUserSQL uid $ getPrimitive primName

deletePrimitiveR :: Text -> Handler Value
deletePrimitiveR primName = jsonResult $ do
    uid <- requireUser
    validateName primName
    runUserSQL uid $ do
        primId <- getPrimitiveId primName
                  `orElseM` fail "The primitive type does not exist."
        deleteDecl primId
