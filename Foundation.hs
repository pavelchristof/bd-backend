module Foundation where

import ClassyPrelude.Yesod
import Database.Persist.Sql

-- | The application.
data App = App ConnectionPool

-- Routes.

mkYesodData "App" [parseRoutes|
 /auth/login/    Login GET POST
 /auth/logout/   Logout POST
 /auth/register/ Register POST
 /auth/delete/   Delete POST
 |]

-- JSON error messages.

errorMsg :: ErrorResponse -> Text
errorMsg NotFound = "Not found."
errorMsg (InternalError err) = err
errorMsg (InvalidArgs errs) = unlines errs
errorMsg NotAuthenticated = "Not authenticated."
errorMsg (PermissionDenied err) = err
errorMsg (BadMethod _) = "Bad method."

jsonForSuccess :: ToJSON a => a -> Value
jsonForSuccess val = object
    [ "ok" .= True
    , "value" .= toJSON val ]

jsonForError :: Text -> Value
jsonForError err = object
    [ "ok" .= False
    , "message" .= err ]

jsonResult :: ToJSON a => Handler a -> Handler Value
jsonResult = fmap jsonForSuccess

instance Yesod App where
    errorHandler = return . toTypedContent . jsonForError . errorMsg

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend

    runDB action = do
        App pool <- getYesod
        runSqlPool action pool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
