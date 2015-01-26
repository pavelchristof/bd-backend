import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql

import Imports
import Sql.Schema
import Handlers.Auth

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = runStderrLoggingT $ do
    pool <- createPostgresqlPool connectionString connections
    runSqlPool createUsers pool
    liftIO $ warp 3000 $ App pool
  where
    connectionString =
           "host=localhost "
        <> "port=5432 "
        <> "user=yesod "
        <> "password=yesod "
        <> "dbname=yesod "
    connections = 10
