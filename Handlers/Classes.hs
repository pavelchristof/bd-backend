module Handlers.Classes where

import Imports

getClassR :: Text -> Handler Value
getClassR className' = jsonResult $ do
    className <- parseName className'
                 `orElse` fail "Invalid class name."
    return $ prettyPrint className
