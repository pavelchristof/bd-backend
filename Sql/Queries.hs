module Sql.Queries where

import Imports
import Sql.User

getTypeId :: Text -> UserSQL (Maybe TypeId)
getTypeId name = queryMaybeSingle
    "SELECT id \
    \FROM $.Types \
    \WHERE name = ?"
    [toPersistValue name]

getClassId :: Text -> UserSQL (Maybe ClassId)
getClassId name = queryMaybeSingle
    "SELECT $.Classes.id \
    \FROM $.Classes \
    \  INNER JOIN $.Types ON $.Types.id = $.Classes.id \
    \WHERE $.Types.name = ?"
    [toPersistValue name]

getFieldId :: ClassId -> Text -> UserSQL (Maybe FieldId)
getFieldId classId fieldIdent = queryMaybeSingle
    "SELECT $.Fields.id \
    \FROM $.Fields \
    \WHERE $.Fields.class = ? AND $.Fields.name = ?"
    [toPersistValue classId, toPersistValue fieldIdent]

getClass :: Text -> UserSQL (Maybe DBClass)
getClass name = queryMaybeSingle
    "SELECT $.Declarations.file, $.Types.name, $.Classes.isStruct \
    \FROM $.Classes \
    \  INNER JOIN $.Types ON $.Types.id = $.Classes.id \
    \  INNER JOIN $.Declarations ON $.Declarations.id = $.Classes.id \
    \WHERE $.Types.name = ?"
    [toPersistValue name]

getClasses :: UserSQL [DBClass]
getClasses = queryMany
    "SELECT $.Declarations.file, $.Types.name, $.Classes.isStruct \
    \FROM $.Classes \
    \  INNER JOIN $.Types ON $.Types.id = $.Classes.id \
    \  INNER JOIN $.Declarations ON $.Declarations.id = $.Classes.id"
    []

getFields :: ClassId -> UserSQL [DBField]
getFields cid = queryMany
    "SELECT $.Declarations.file, $.Fields.name, $.Fields.static, $.Types.name \
    \FROM $.Fields \
    \  INNER JOIN $.Types ON $.Fields.type = $.Types.id \
    \  INNER JOIN $.Declarations ON $.Declarations.id = $.Fields.id \
    \WHERE $.Fields.class = ?"
    [toPersistValue cid]

getField :: ClassId -> Text -> UserSQL (Maybe DBField)
getField cid name = queryMaybeSingle
    "SELECT $.Declarations.file, $.Fields.name, $.Fields.static, $.Types.name \
    \FROM $.Fields \
    \  INNER JOIN $.Types ON $.Fields.type = $.Types.id \
    \  INNER JOIN $.Declarations ON $.Declarations.id = $.Fields.id \
    \WHERE $.Fields.class = ? AND $.Fields.name = ?"
    [toPersistValue cid, toPersistValue name]

getMethodArgs :: MethodId -> UserSQL DBMethodArgs
getMethodArgs methId = DBMethodArgs <$> queryMany
    "SELECT $.Types.name \
    \FROM $.Arguments \
    \  INNER JOIN $.Types ON $.Types.id = $.Arguments.type \
    \WHERE $.Arguments.func = ? \
    \ORDER BY $.Arguments.index"
    [toPersistValue methId]

getMethods :: ClassId -> UserSQL [(DBMethod, DBMethodArgs)]
getMethods classId = do
    methods <- queryMany
      "SELECT $.Methods.id, $.Declarations.file, $.Methods.name, $.Methods.static, $.Types.name \
      \FROM $.Methods \
      \  INNER JOIN $.Declarations ON $.Declarations.id = $.Methods.id \
      \  INNER JOIN $.Types ON $.Types.id = $.Methods.returnType \
      \WHERE $.Methods.class = ?"
      [toPersistValue classId]
    args <- mapM (getMethodArgs . methodId) methods
    return $ zip methods args

getMethod :: MethodId -> UserSQL (Maybe (DBMethod, DBMethodArgs))
getMethod methId = do
    mbMethod <- queryMaybeSingle
      "SELECT $.Methods.id, $.Declarations.file, $.Methods.name, $.Methods.static, $.Types.name \
      \FROM $.Methods \
      \  INNER JOIN $.Declarations ON $.Declarations.id = $.Methods.id \
      \  INNER JOIN $.Types ON $.Types.id = $.Methods.returnType \
      \WHERE $.Methods.id = ?"
      [toPersistValue methId]
    forM mbMethod $ \method -> do
        args <- getMethodArgs methId
        return (method, args)

createDecl :: DBDecl -> UserSQL DeclId
createDecl decl = querySingle
    "INSERT INTO $.Declarations (file) \
    \VALUES (?) \
    \RETURNING id"
    [toPersistValue (declFile decl)]

createType :: DBType -> UserSQL TypeId
createType ty = do
    typeId <- createDecl (typeDecl ty)
    execStmt
      "INSERT INTO $.Types (id, name) \
      \VALUES (?, ?)"
      [toPersistValue typeId, toPersistValue (typeName ty)]
    return typeId

createValue :: DBValue -> UserSQL ValueId
createValue val = do
    vid <- createDecl (valueDecl val)
    execStmt
      "INSERT INTO $.Values (id, name) \
      \VALUES (?, ?)"
      [toPersistValue vid, toPersistValue (valueIdent val)]
    return vid

createClass :: DBClass -> UserSQL ClassId
createClass cl = do
    classId <- createType (classType cl)
    execStmt
      "INSERT INTO $.Classes (id, isStruct) \
      \VALUES (?, ?)"
      [toPersistValue classId, toPersistValue (classIsStruct cl)]
    return classId

createField :: ClassId -> DBField -> UserSQL FieldId
createField classId f = do
    typeId <- getTypeId (fieldTypeName f)
              `orElseM` fail "Invalid field type."
    fieldId <- createValue (fieldValue f)
    execStmt
      "INSERT INTO $.Fields (id, name, class, static, type) \
      \VALUES (?, ?, ?, ?, ?)"
      [ toPersistValue fieldId
      , toPersistValue (valueIdent $ fieldValue f)
      , toPersistValue classId
      , toPersistValue (fieldStatic f)
      , toPersistValue typeId
      ]
    return fieldId

createMethod :: ClassId -> DBMethod -> DBMethodArgs -> UserSQL MethodId
createMethod classId m (DBMethodArgs args) = do
    mId <- createValue (methodValue m)
    retType <- getTypeId (methodReturnType m)
               `orElseM` fail ("Invalid method return type: "
                              <> unpack (methodReturnType m))

    execStmt
      "INSERT INTO $.Methods (id, name, class, static, returnType) \
      \VALUES (?, ?, ?, ?, ?)"
      [ toPersistValue mId
      , toPersistValue (valueIdent $ methodValue m)
      , toPersistValue classId
      , toPersistValue (methodStatic m)
      , toPersistValue retType
      ]

    forM_ (zip args [0..]) $ \(arg, idx :: Int) -> do
        typeId <- getTypeId arg
                  `orElseM` fail ("Invalid method argument type: "
                                 <> unpack arg)
        execStmt
          "INSERT INTO $.Arguments (func, index, type) \
          \VALUES (?, ?, ?)"
          [toPersistValue mId, toPersistValue idx, toPersistValue typeId]

    return mId

deleteDecl :: DeclId -> UserSQL ()
deleteDecl declId = execStmt
    "DELETE FROM $.Declarations \
    \WHERE id = ?"
    [toPersistValue declId]
