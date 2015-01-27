import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql

import Handlers.Auth
import Handlers.Classes
import Handlers.Primitives
import Handlers.Enums
import Imports

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = runStderrLoggingT $ do
    pool <- createPostgresqlPool connectionString connections
    runSqlPool createDB pool
    liftIO $ warp 3000 $ App pool
  where
    connectionString =
           "host=localhost "
        <> "port=5432 "
        <> "user=yesod "
        <> "password=yesod "
        <> "dbname=yesod "
    connections = 10
