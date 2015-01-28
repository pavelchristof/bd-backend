module Sql.Rels where

import Imports
import Sql.User

getInherits :: UserSQL [(Text, Text)]
getInherits = queryMany
    "SELECT who.name, whom.name \
    \FROM $.Inherits\
    \  INNER JOIN $.Types who ON who.id = $.Inherits.parent \
    \  INNER JOIN $.Types whom ON whom.id = $.Inherits.child"
    []

createInherits :: ClassId -> ClassId -> UserSQL ()
createInherits who whom = execStmt
    "INSERT INTO $.Inherits (parent, child) \
    \VALUES (?, ?)"
    [toPersistValue who, toPersistValue whom]

getCalls :: UserSQL [(MethodId, MethodId)]
getCalls = queryMany
    "SELECT $.Calls.caller, $.Calls.callee \
    \FROM $.Calls"
    []

createCalls :: MethodId -> MethodId -> UserSQL ()
createCalls who whom = execStmt
    "INSERT INTO $.Calls (caller, callee) \
    \VALUES (?, ?)"
    [toPersistValue who, toPersistValue whom]

getReads :: UserSQL [(MethodId, (Text, Text))]
getReads = queryMany
    "SELECT $.Reads.func, cl.name, $.Fields.name \
    \FROM $.Reads \
    \  INNER JOIN $.Fields ON $.Fields.id = $.Reads.var \
    \  INNER JOIN $.Types cl ON cl.id = $.Fields.class"
    []

createReads :: MethodId -> FieldId -> UserSQL ()
createReads who whom = execStmt
    "INSERT INTO $.Reads (func, var) \
    \VALUES (?, ?)"
    [toPersistValue who, toPersistValue whom]

getWrites :: UserSQL [(MethodId, (Text, Text))]
getWrites = queryMany
    "SELECT $.Writes.func, cl.name, $.Fields.name \
    \FROM $.Writes \
    \  INNER JOIN $.Fields ON $.Fields.id = $.Writes.var \
    \  INNER JOIN $.Types cl ON cl.id = $.Fields.class"
    []

createWrites :: MethodId -> FieldId -> UserSQL ()
createWrites who whom = execStmt
    "INSERT INTO $.Writes (func, var) \
    \VALUES (?, ?)"
    [toPersistValue who, toPersistValue whom]
