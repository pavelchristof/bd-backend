module Forms where

import Imports

identField :: Field Handler Text
identField = checkBool
    isIdentOk
    ("Invalid identifier." :: Text)
    textField

nameField :: Field Handler Text
nameField = checkBool
    isNameOk
    ("Invalid name." :: Text)
    textField

identListField :: Field Handler [Text]
identListField = Field
    { fieldParse = \txts _ -> do
        mapM_ validateIdent txts
        return $ Right $ Just txts
    , fieldView = \_ _ _ _ _ -> return ()
    , fieldEnctype = UrlEncoded
    }

nameListField :: Field Handler [Text]
nameListField = Field
    { fieldParse = \txts _ -> do
        mapM_ validateName txts
        return $ Right $ Just txts
    , fieldView = \_ _ _ _ _ -> return ()
    , fieldEnctype = UrlEncoded
    }

declForm :: FormInput Handler DBDecl
declForm = DBDecl
    <$> iopt textField "file"

valueForm :: FormInput Handler DBValue
valueForm = DBValue
    <$> declForm
    <*> ireq identField "ident"

typeForm :: FormInput Handler DBType
typeForm = DBType
    <$> declForm
    <*> ireq nameField "name"

classForm :: FormInput Handler DBClass
classForm = DBClass
    <$> typeForm
    <*> ireq checkBoxField "isStruct"

fieldForm :: FormInput Handler DBField
fieldForm = DBField
    <$> valueForm
    <*> ireq checkBoxField "static"
    <*> ireq nameField "type"

methodForm :: FormInput Handler (DBMethod, DBMethodArgs)
methodForm = (,) <$> body <*> args
  where
    body = DBMethod
        <$> pure 0
        <*> valueForm
        <*> ireq checkBoxField "static"
        <*> ireq nameField "returnType"
    args = DBMethodArgs
        <$> ireq nameListField "argTypes"

enumForm :: FormInput Handler DBEnum
enumForm = DBEnum
    <$> typeForm
    <*> ireq identListField "items"

inheritsForm :: FormInput Handler (Text, Text)
inheritsForm = (,)
    <$> ireq nameField "who"
    <*> ireq nameField "whom"

callsForm :: FormInput Handler (MethodId, MethodId)
callsForm = (,)
    <$> ireq intField "who"
    <*> ireq intField "whom"

readsOrWritesForm :: FormInput Handler (MethodId, Text, Text)
readsOrWritesForm = (,,)
    <$> ireq intField "who"
    <*> ireq nameField "whomClass"
    <*> ireq identField "whomField"
