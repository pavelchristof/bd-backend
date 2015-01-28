import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors

import Handlers.Auth
import Handlers.Classes
import Handlers.Primitives
import Handlers.Enums
import Handlers.Rels
import Imports

mkYesodDispatch "App" resourcesApp

corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy {
    corsOrigins = Just (["http://localhost:8000"], True),
    corsMethods = ["GET", "POST", "DELETE"]
}

main :: IO ()
main = runStderrLoggingT $ do
    pool <- createPostgresqlPool connectionString connections
    runSqlPool createDB pool
    liftIO $ do
        app <- toWaiApp $ App pool
        run 3000 $ cors (const (Just corsPolicy)) app
  where
    connectionString =
           "host=localhost "
        <> "port=5432 "
        <> "user=yesod "
        <> "password=yesod "
        <> "dbname=yesod "
    connections = 10
