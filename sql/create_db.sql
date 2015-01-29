CREATE TABLE IF NOT EXISTS Users (
    id serial PRIMARY KEY,
    username varchar(256) UNIQUE,
    password varchar(256)
);

CREATE OR REPLACE FUNCTION isUserOk(userId int) RETURNS SETOF bool
AS $$
DECLARE
BEGIN
  RETURN QUERY SELECT EXISTS (SELECT 1 FROM Users WHERE id=userId);
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION createUserSchema() RETURNS TRIGGER
AS $$
DECLARE
    query TEXT;
    prefix TEXT;

BEGIN
    query := '
    CREATE TABLE $.Declarations (
      id serial PRIMARY KEY,
      file varchar(1024)
    );

    CREATE TABLE $.Types (
      id int PRIMARY KEY REFERENCES $.Declarations(id) ON DELETE CASCADE,
      name varchar(1024) UNIQUE NOT NULL
    );

    CREATE TABLE $.Values (
      id int REFERENCES $.Declarations(id) ON DELETE CASCADE,
      name varchar(1024) NOT NULL,
      PRIMARY KEY (id, name)
    );

    -- Types.

    CREATE TABLE $.Primitives (
      id int PRIMARY KEY REFERENCES $.Types(id) ON DELETE CASCADE
    );

    CREATE TABLE $.Classes (
      id int PRIMARY KEY REFERENCES $.Types(id) ON DELETE CASCADE,
      isStruct bool NOT NULL
    );

    CREATE TABLE $.Enumerations (
      id int PRIMARY KEY REFERENCES $.Types(id) ON DELETE CASCADE
    );

    -- Values.

    CREATE TABLE $.Fields (
      id int PRIMARY KEY,
      name varchar(1024) NOT NULL,
      class int NOT NULL REFERENCES $.Classes(id) ON DELETE CASCADE,
      static bool NOT NULL,
      type int NOT NULL REFERENCES $.Types(id) ON DELETE CASCADE,
      FOREIGN KEY (id, name) REFERENCES $.Values(id, name) ON DELETE CASCADE,
      unique (name, class)
    );

    CREATE TABLE $.Enumerators (
      id int PRIMARY KEY,
      name varchar(1024) NOT NULL,
      enum int NOT NULL REFERENCES $.Enumerations(id) ON DELETE CASCADE,
      FOREIGN KEY (id, name) REFERENCES $.Values(id, name) ON DELETE CASCADE,
      unique (name, enum)
    );

    CREATE TABLE $.Methods (
      id int PRIMARY KEY,
      name varchar(1024) NOT NULL,
      class int REFERENCES $.Classes(id) ON DELETE CASCADE,
      static bool NOT NULL,
      returnType int NOT NULL REFERENCES $.Types(id) ON DELETE CASCADE,
      FOREIGN KEY (id, name) REFERENCES $.Values(id, name) ON DELETE CASCADE
    );

    CREATE TABLE $.Arguments (
      func int REFERENCES $.Methods(id) ON DELETE CASCADE,
      index int NOT NULL,
      type int NOT NULL REFERENCES $.Types(id) ON DELETE CASCADE,
      PRIMARY KEY (func, index)
    );

    -- Relations.

    CREATE TABLE $.Inherits (
      parent int REFERENCES $.Classes(id) ON DELETE CASCADE,
      child int REFERENCES $.Classes(id) ON DELETE CASCADE,
      PRIMARY KEY (parent, child)
    );

    CREATE TABLE $.Calls (
      caller int REFERENCES $.Methods(id) ON DELETE CASCADE,
      callee int REFERENCES $.Methods(id) ON DELETE CASCADE,
      PRIMARY KEY (caller, callee)
    );

    CREATE TABLE $.Reads (
      func int REFERENCES $.Methods(id) ON DELETE CASCADE,
      var int REFERENCES $.Fields(id) ON DELETE CASCADE,
      PRIMARY KEY (func, var)
    );

    CREATE TABLE $.Writes (
      func int REFERENCES $.Methods(id) ON DELETE CASCADE,
      var int REFERENCES $.Fields(id) ON DELETE CASCADE,
      PRIMARY KEY (func, var)
    );';

    prefix := 'user_' || (NEW.id :: TEXT) || '_';
    EXECUTE replace(query, '$.', prefix);
    RETURN NULL;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION dropUserSchema() RETURNS TRIGGER
AS $$
DECLARE
    query TEXT;
    prefix TEXT;

BEGIN
    query := '
    drop table $.Writes;
    drop table $.Reads;
    drop table $.Calls;
    drop table $.Inherits;
    drop table $.Arguments;
    drop table $.Methods;
    drop table $.Enumerators;
    drop table $.Fields;
    drop table $.Enumerations;
    drop table $.Classes;
    drop table $.Primitives;
    drop table $.Values;
    drop table $.Types;
    drop table $.Declarations;
    ';

    prefix := 'user_' || (OLD.id :: TEXT) || '_';
    EXECUTE replace(query, '$.', prefix);
    RETURN NULL;
END
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS userCreated ON Users;
CREATE TRIGGER userCreated
  AFTER INSERT ON Users
  FOR EACH ROW
  EXECUTE PROCEDURE createUserSchema();

DROP TRIGGER IF EXISTS userDeleted ON Users;
CREATE TRIGGER userDeleted
  AFTER DELETE ON Users
  FOR EACH ROW
  EXECUTE PROCEDURE dropUserSchema();
