module Handlers.Rels where

import Forms
import Imports
import Session.User
import Sql.Classes
import Sql.Rels
import Sql.User

getClassId' :: Text -> UserSQL ClassId
getClassId' name = getClassId name
    `orElseM` fail ("Class " <> unpack name <> " does not exist.")

getRelInheritsR :: Handler Value
getRelInheritsR = jsonResult $ do
    uid <- requireUser
    runUserSQL uid getInherits

postRelInheritsR :: Handler Value
postRelInheritsR = jsonResult $ do
    uid <- requireUser
    (who, whom) <- runInputPost inheritsForm
    runUserSQL uid $ do
        whoId <- getClassId' who
        whomId <- getClassId' whom
        createInherits whoId whomId

getRelCallsR :: Handler Value
getRelCallsR = jsonResult $ do
    uid <- requireUser
    runUserSQL uid getCalls

postRelCallsR :: Handler Value
postRelCallsR = jsonResult $ do
    uid <- requireUser
    (who, whom) <- runInputPost callsForm
    runUserSQL uid $ createCalls who whom

getRelReadsR :: Handler Value
getRelReadsR = jsonResult $ do
    uid <- requireUser
    runUserSQL uid getReads

postRelReadsR :: Handler Value
postRelReadsR = jsonResult $ do
    uid <- requireUser
    (who, className, ident) <- runInputPost readsOrWritesForm
    runUserSQL uid $ do
        classId <- getClassId' className
        fieldId <- getFieldId classId ident
                   `orElseM` fail "The field does not exist."
        createReads who fieldId

getRelWritesR :: Handler Value
getRelWritesR = jsonResult $ do
    uid <- requireUser
    runUserSQL uid getWrites

postRelWritesR :: Handler Value
postRelWritesR = jsonResult $ do
    uid <- requireUser
    (who, className, ident) <- runInputPost readsOrWritesForm
    runUserSQL uid $ do
        classId <- getClassId' className
        fieldId <- getFieldId classId ident
                   `orElseM` fail "The field does not exist."
        createWrites who fieldId
