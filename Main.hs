import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql
import Network.Wai.Handler.SCGI
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
    -- Do not create db in CGI mode
    -- runSqlPool createDB pool
    liftIO $ do
        app <- toWaiApp $ App pool
        run $ cors (const (Just corsPolicy)) app
  where
    connectionString =
           "host=labdb "
        <> "port=5432 "
        <> "user=pn347193 "
        <> "password=SXZRR5Gf "
        <> "dbname=bd "
    connections = 10
