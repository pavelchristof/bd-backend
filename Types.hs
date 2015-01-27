module Types (
    UserId,
    Ident,
    Name,
    prettyPrint,
    parseName
    ) where

import ClassyPrelude.Yesod

import Language.Java.Parser
import Language.Java.Pretty
import Language.Java.Syntax
import Text.Parsec

type UserId = Int

toRight :: Either a b -> Maybe b
toRight (Left _) = Nothing
toRight (Right x) = Just x

parseName :: Text -> Maybe Name
parseName = toRight . parser (name <* eof) . unpack

-- instance PathPiece Name where
--     fromPathPiece = toRight . parser (name <* eof) . unpack
--     toPathPiece = pack . prettyPrint

-- instance Read Name where
