--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = pn347193, pg_catalog;

ALTER TABLE ONLY pn347193.user_1_writes DROP CONSTRAINT user_1_writes_var_fkey;
ALTER TABLE ONLY pn347193.user_1_writes DROP CONSTRAINT user_1_writes_func_fkey;
ALTER TABLE ONLY pn347193.user_1_values DROP CONSTRAINT user_1_values_id_fkey;
ALTER TABLE ONLY pn347193.user_1_types DROP CONSTRAINT user_1_types_id_fkey;
ALTER TABLE ONLY pn347193.user_1_reads DROP CONSTRAINT user_1_reads_var_fkey;
ALTER TABLE ONLY pn347193.user_1_reads DROP CONSTRAINT user_1_reads_func_fkey;
ALTER TABLE ONLY pn347193.user_1_primitives DROP CONSTRAINT user_1_primitives_id_fkey;
ALTER TABLE ONLY pn347193.user_1_methods DROP CONSTRAINT user_1_methods_returntype_fkey;
ALTER TABLE ONLY pn347193.user_1_methods DROP CONSTRAINT user_1_methods_id_fkey;
ALTER TABLE ONLY pn347193.user_1_methods DROP CONSTRAINT user_1_methods_class_fkey;
ALTER TABLE ONLY pn347193.user_1_inherits DROP CONSTRAINT user_1_inherits_parent_fkey;
ALTER TABLE ONLY pn347193.user_1_inherits DROP CONSTRAINT user_1_inherits_child_fkey;
ALTER TABLE ONLY pn347193.user_1_fields DROP CONSTRAINT user_1_fields_type_fkey;
ALTER TABLE ONLY pn347193.user_1_fields DROP CONSTRAINT user_1_fields_id_fkey;
ALTER TABLE ONLY pn347193.user_1_fields DROP CONSTRAINT user_1_fields_class_fkey;
ALTER TABLE ONLY pn347193.user_1_enumerators DROP CONSTRAINT user_1_enumerators_id_fkey;
ALTER TABLE ONLY pn347193.user_1_enumerators DROP CONSTRAINT user_1_enumerators_enum_fkey;
ALTER TABLE ONLY pn347193.user_1_enumerations DROP CONSTRAINT user_1_enumerations_id_fkey;
ALTER TABLE ONLY pn347193.user_1_classes DROP CONSTRAINT user_1_classes_id_fkey;
ALTER TABLE ONLY pn347193.user_1_calls DROP CONSTRAINT user_1_calls_caller_fkey;
ALTER TABLE ONLY pn347193.user_1_calls DROP CONSTRAINT user_1_calls_callee_fkey;
ALTER TABLE ONLY pn347193.user_1_arguments DROP CONSTRAINT user_1_arguments_type_fkey;
ALTER TABLE ONLY pn347193.user_1_arguments DROP CONSTRAINT user_1_arguments_func_fkey;
DROP TRIGGER userdeleted ON pn347193.users;
DROP TRIGGER usercreated ON pn347193.users;
ALTER TABLE ONLY pn347193.users DROP CONSTRAINT users_username_key;
ALTER TABLE ONLY pn347193.users DROP CONSTRAINT users_pkey;
ALTER TABLE ONLY pn347193.user_1_writes DROP CONSTRAINT user_1_writes_pkey;
ALTER TABLE ONLY pn347193.user_1_values DROP CONSTRAINT user_1_values_pkey;
ALTER TABLE ONLY pn347193.user_1_types DROP CONSTRAINT user_1_types_pkey;
ALTER TABLE ONLY pn347193.user_1_types DROP CONSTRAINT user_1_types_name_key;
ALTER TABLE ONLY pn347193.user_1_reads DROP CONSTRAINT user_1_reads_pkey;
ALTER TABLE ONLY pn347193.user_1_primitives DROP CONSTRAINT user_1_primitives_pkey;
ALTER TABLE ONLY pn347193.user_1_methods DROP CONSTRAINT user_1_methods_pkey;
ALTER TABLE ONLY pn347193.user_1_inherits DROP CONSTRAINT user_1_inherits_pkey;
ALTER TABLE ONLY pn347193.user_1_fields DROP CONSTRAINT user_1_fields_pkey;
ALTER TABLE ONLY pn347193.user_1_fields DROP CONSTRAINT user_1_fields_name_class_key;
ALTER TABLE ONLY pn347193.user_1_enumerators DROP CONSTRAINT user_1_enumerators_pkey;
ALTER TABLE ONLY pn347193.user_1_enumerators DROP CONSTRAINT user_1_enumerators_name_enum_key;
ALTER TABLE ONLY pn347193.user_1_enumerations DROP CONSTRAINT user_1_enumerations_pkey;
ALTER TABLE ONLY pn347193.user_1_declarations DROP CONSTRAINT user_1_declarations_pkey;
ALTER TABLE ONLY pn347193.user_1_classes DROP CONSTRAINT user_1_classes_pkey;
ALTER TABLE ONLY pn347193.user_1_calls DROP CONSTRAINT user_1_calls_pkey;
ALTER TABLE ONLY pn347193.user_1_arguments DROP CONSTRAINT user_1_arguments_pkey;
ALTER TABLE pn347193.users ALTER COLUMN id DROP DEFAULT;
ALTER TABLE pn347193.user_1_declarations ALTER COLUMN id DROP DEFAULT;
DROP SEQUENCE pn347193.users_id_seq;
DROP TABLE pn347193.users;
DROP TABLE pn347193.user_1_writes;
DROP TABLE pn347193.user_1_values;
DROP TABLE pn347193.user_1_types;
DROP TABLE pn347193.user_1_reads;
DROP TABLE pn347193.user_1_primitives;
DROP TABLE pn347193.user_1_methods;
DROP TABLE pn347193.user_1_inherits;
DROP TABLE pn347193.user_1_fields;
DROP TABLE pn347193.user_1_enumerators;
DROP TABLE pn347193.user_1_enumerations;
DROP SEQUENCE pn347193.user_1_declarations_id_seq;
DROP TABLE pn347193.user_1_declarations;
DROP TABLE pn347193.user_1_classes;
DROP TABLE pn347193.user_1_calls;
DROP TABLE pn347193.user_1_arguments;
DROP FUNCTION pn347193.isuserok(userid integer);
DROP FUNCTION pn347193.dropuserschema();
DROP FUNCTION pn347193.createuserschema();
DROP EXTENSION plpgsql;
DROP SCHEMA pn347193;
--
-- Name: pn347193; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA pn347193;


--
-- Name: SCHEMA pn347193; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA pn347193 IS 'standard pn347193 schema';


--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = pn347193, pg_catalog;

--
-- Name: createuserschema(); Type: FUNCTION; Schema: pn347193; Owner: -
--

CREATE FUNCTION createuserschema() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$
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
$_$;


--
-- Name: dropuserschema(); Type: FUNCTION; Schema: pn347193; Owner: -
--

CREATE FUNCTION dropuserschema() RETURNS trigger
    LANGUAGE plpgsql
    AS $_$
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
$_$;


--
-- Name: isuserok(integer); Type: FUNCTION; Schema: pn347193; Owner: -
--

CREATE FUNCTION isuserok(userid integer) RETURNS SETOF boolean
    LANGUAGE plpgsql
    AS $$
DECLARE
BEGIN
  RETURN QUERY SELECT EXISTS (SELECT 1 FROM Users WHERE id=userId);
END
$$;


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: user_1_arguments; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_arguments (
    func integer NOT NULL,
    index integer NOT NULL,
    type integer NOT NULL
);


--
-- Name: user_1_calls; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_calls (
    caller integer NOT NULL,
    callee integer NOT NULL
);


--
-- Name: user_1_classes; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_classes (
    id integer NOT NULL,
    isstruct boolean NOT NULL
);


--
-- Name: user_1_declarations; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_declarations (
    id integer NOT NULL,
    file character varying(1024)
);


--
-- Name: user_1_declarations_id_seq; Type: SEQUENCE; Schema: pn347193; Owner: -
--

CREATE SEQUENCE user_1_declarations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: user_1_declarations_id_seq; Type: SEQUENCE OWNED BY; Schema: pn347193; Owner: -
--

ALTER SEQUENCE user_1_declarations_id_seq OWNED BY user_1_declarations.id;


--
-- Name: user_1_enumerations; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_enumerations (
    id integer NOT NULL
);


--
-- Name: user_1_enumerators; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_enumerators (
    id integer NOT NULL,
    name character varying(1024) NOT NULL,
    enum integer NOT NULL
);


--
-- Name: user_1_fields; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_fields (
    id integer NOT NULL,
    name character varying(1024) NOT NULL,
    class integer NOT NULL,
    static boolean NOT NULL,
    type integer NOT NULL
);


--
-- Name: user_1_inherits; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_inherits (
    parent integer NOT NULL,
    child integer NOT NULL
);


--
-- Name: user_1_methods; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_methods (
    id integer NOT NULL,
    name character varying(1024) NOT NULL,
    class integer,
    static boolean NOT NULL,
    returntype integer NOT NULL
);


--
-- Name: user_1_primitives; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_primitives (
    id integer NOT NULL
);


--
-- Name: user_1_reads; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_reads (
    func integer NOT NULL,
    var integer NOT NULL
);


--
-- Name: user_1_types; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_types (
    id integer NOT NULL,
    name character varying(1024) NOT NULL
);


--
-- Name: user_1_values; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_values (
    id integer NOT NULL,
    name character varying(1024) NOT NULL
);


--
-- Name: user_1_writes; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE user_1_writes (
    func integer NOT NULL,
    var integer NOT NULL
);


--
-- Name: users; Type: TABLE; Schema: pn347193; Owner: -; Tablespace: 
--

CREATE TABLE users (
    id integer NOT NULL,
    username character varying(256),
    password character varying(256)
);


--
-- Name: users_id_seq; Type: SEQUENCE; Schema: pn347193; Owner: -
--

CREATE SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: pn347193; Owner: -
--

ALTER SEQUENCE users_id_seq OWNED BY users.id;


--
-- Name: id; Type: DEFAULT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_declarations ALTER COLUMN id SET DEFAULT nextval('user_1_declarations_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- Data for Name: user_1_arguments; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_arguments (func, index, type) VALUES (70, 0, 13);
INSERT INTO user_1_arguments (func, index, type) VALUES (75, 0, 15);
INSERT INTO user_1_arguments (func, index, type) VALUES (75, 1, 10);
INSERT INTO user_1_arguments (func, index, type) VALUES (75, 2, 66);
INSERT INTO user_1_arguments (func, index, type) VALUES (79, 0, 66);
INSERT INTO user_1_arguments (func, index, type) VALUES (79, 1, 66);
INSERT INTO user_1_arguments (func, index, type) VALUES (80, 0, 66);
INSERT INTO user_1_arguments (func, index, type) VALUES (80, 1, 66);
INSERT INTO user_1_arguments (func, index, type) VALUES (83, 0, 13);
INSERT INTO user_1_arguments (func, index, type) VALUES (124, 0, 15);
INSERT INTO user_1_arguments (func, index, type) VALUES (124, 1, 15);
INSERT INTO user_1_arguments (func, index, type) VALUES (126, 0, 15);
INSERT INTO user_1_arguments (func, index, type) VALUES (130, 0, 66);
INSERT INTO user_1_arguments (func, index, type) VALUES (135, 0, 66);
INSERT INTO user_1_arguments (func, index, type) VALUES (136, 0, 15);
INSERT INTO user_1_arguments (func, index, type) VALUES (137, 0, 15);


--
-- Data for Name: user_1_calls; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_calls (caller, callee) VALUES (76, 75);
INSERT INTO user_1_calls (caller, callee) VALUES (75, 83);
INSERT INTO user_1_calls (caller, callee) VALUES (124, 124);
INSERT INTO user_1_calls (caller, callee) VALUES (126, 126);
INSERT INTO user_1_calls (caller, callee) VALUES (124, 126);
INSERT INTO user_1_calls (caller, callee) VALUES (127, 124);
INSERT INTO user_1_calls (caller, callee) VALUES (78, 79);
INSERT INTO user_1_calls (caller, callee) VALUES (128, 78);
INSERT INTO user_1_calls (caller, callee) VALUES (128, 137);
INSERT INTO user_1_calls (caller, callee) VALUES (127, 138);
INSERT INTO user_1_calls (caller, callee) VALUES (138, 127);


--
-- Data for Name: user_1_classes; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_classes (id, isstruct) VALUES (59, false);
INSERT INTO user_1_classes (id, isstruct) VALUES (63, false);
INSERT INTO user_1_classes (id, isstruct) VALUES (66, true);
INSERT INTO user_1_classes (id, isstruct) VALUES (82, false);
INSERT INTO user_1_classes (id, isstruct) VALUES (85, false);
INSERT INTO user_1_classes (id, isstruct) VALUES (86, false);
INSERT INTO user_1_classes (id, isstruct) VALUES (87, false);
INSERT INTO user_1_classes (id, isstruct) VALUES (95, false);
INSERT INTO user_1_classes (id, isstruct) VALUES (123, false);


--
-- Data for Name: user_1_declarations; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_declarations (id, file) VALUES (2, 'Test.java');
INSERT INTO user_1_declarations (id, file) VALUES (5, 'Test.java');
INSERT INTO user_1_declarations (id, file) VALUES (10, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (11, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (12, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (13, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (14, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (15, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (18, 'Bool.java');
INSERT INTO user_1_declarations (id, file) VALUES (19, 'Bool.java');
INSERT INTO user_1_declarations (id, file) VALUES (22, 'Bool.java');
INSERT INTO user_1_declarations (id, file) VALUES (23, 'Bool.java');
INSERT INTO user_1_declarations (id, file) VALUES (25, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (26, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (27, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (39, 'Bool.java');
INSERT INTO user_1_declarations (id, file) VALUES (40, 'Bool.java');
INSERT INTO user_1_declarations (id, file) VALUES (42, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (43, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (44, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (45, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (46, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (47, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (48, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (49, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (50, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (51, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (52, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (53, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (56, 'Bool.java');
INSERT INTO user_1_declarations (id, file) VALUES (57, 'Bool.java');
INSERT INTO user_1_declarations (id, file) VALUES (58, 'Bool.java');
INSERT INTO user_1_declarations (id, file) VALUES (59, 'File.java');
INSERT INTO user_1_declarations (id, file) VALUES (60, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (61, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (62, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (63, 'FooBar.java');
INSERT INTO user_1_declarations (id, file) VALUES (64, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (65, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (66, 'Vector3D.java');
INSERT INTO user_1_declarations (id, file) VALUES (67, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (68, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (69, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (70, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (72, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (73, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (74, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (75, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (76, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (77, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (78, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (79, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (80, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (81, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (82, 'FooBarBase.java');
INSERT INTO user_1_declarations (id, file) VALUES (83, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (84, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (85, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (86, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (87, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (88, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (89, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (90, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (91, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (92, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (93, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (95, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (96, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (97, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (98, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (99, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (100, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (101, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (102, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (103, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (104, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (105, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (106, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (107, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (108, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (109, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (110, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (111, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (112, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (113, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (114, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (115, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (116, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (117, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (118, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (119, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (120, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (121, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (122, 'UnorderedAlphabet.java');
INSERT INTO user_1_declarations (id, file) VALUES (123, 'src/some/file/in/some/language.txt');
INSERT INTO user_1_declarations (id, file) VALUES (124, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (126, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (127, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (128, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (130, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (135, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (136, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (137, NULL);
INSERT INTO user_1_declarations (id, file) VALUES (138, NULL);


--
-- Name: user_1_declarations_id_seq; Type: SEQUENCE SET; Schema: pn347193; Owner: -
--

SELECT pg_catalog.setval('user_1_declarations_id_seq', 138, true);


--
-- Data for Name: user_1_enumerations; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_enumerations (id) VALUES (56);
INSERT INTO user_1_enumerations (id) VALUES (96);


--
-- Data for Name: user_1_enumerators; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_enumerators (id, name, enum) VALUES (57, 'True', 56);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (58, 'False', 56);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (97, 'Q', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (98, 'W', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (99, 'E', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (100, 'R', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (101, 'T', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (102, 'Y', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (103, 'U', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (104, 'I', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (105, 'O', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (106, 'P', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (107, 'A', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (108, 'S', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (109, 'D', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (110, 'F', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (111, 'G', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (112, 'H', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (113, 'J', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (114, 'K', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (115, 'L', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (116, 'Z', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (117, 'X', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (118, 'C', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (119, 'V', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (120, 'B', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (121, 'N', 96);
INSERT INTO user_1_enumerators (id, name, enum) VALUES (122, 'M', 96);


--
-- Data for Name: user_1_fields; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_fields (id, name, class, static, type) VALUES (60, 'content', 59, false, 13);
INSERT INTO user_1_fields (id, name, class, static, type) VALUES (61, 'name', 59, false, 13);
INSERT INTO user_1_fields (id, name, class, static, type) VALUES (62, 'size', 59, false, 15);
INSERT INTO user_1_fields (id, name, class, static, type) VALUES (64, 'foo_', 63, true, 59);
INSERT INTO user_1_fields (id, name, class, static, type) VALUES (65, 'bar_', 63, false, 63);
INSERT INTO user_1_fields (id, name, class, static, type) VALUES (67, 'x', 66, false, 15);
INSERT INTO user_1_fields (id, name, class, static, type) VALUES (68, 'y', 66, false, 15);
INSERT INTO user_1_fields (id, name, class, static, type) VALUES (69, 'z', 66, false, 15);
INSERT INTO user_1_fields (id, name, class, static, type) VALUES (88, 'testField', 86, false, 15);
INSERT INTO user_1_fields (id, name, class, static, type) VALUES (90, 'anotherField', 86, false, 13);
INSERT INTO user_1_fields (id, name, class, static, type) VALUES (92, 'a', 87, false, 15);


--
-- Data for Name: user_1_inherits; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_inherits (parent, child) VALUES (63, 82);
INSERT INTO user_1_inherits (parent, child) VALUES (86, 85);
INSERT INTO user_1_inherits (parent, child) VALUES (85, 87);
INSERT INTO user_1_inherits (parent, child) VALUES (86, 59);
INSERT INTO user_1_inherits (parent, child) VALUES (95, 86);


--
-- Data for Name: user_1_methods; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (70, 'open', 59, false, 56);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (73, 'close', 59, false, 72);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (74, 'maxSize', 59, true, 15);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (75, 'foo', 63, true, 11);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (76, 'getBar', 63, false, 63);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (78, 'length', 66, false, 77);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (79, 'dot', 66, true, 77);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (80, 'cross', 66, true, 66);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (81, 'reset', 66, false, 72);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (83, 'fooBase', 82, false, 15);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (84, 'read', 59, false, 13);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (89, 'modifyTestField', 86, false, 72);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (91, 'readAnotherField', 86, false, 13);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (93, 'getA', 87, false, 15);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (124, 'func', 123, true, 63);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (126, 'fibb', 123, true, 15);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (127, 'abc', 123, false, 72);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (128, 'normalize', 66, false, 66);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (130, 'sub', 66, false, 66);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (135, 'add', 66, false, 66);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (136, 'div', 66, false, 66);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (137, 'mul', 66, false, 66);
INSERT INTO user_1_methods (id, name, class, static, returntype) VALUES (138, 'def', 123, true, 72);


--
-- Data for Name: user_1_primitives; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_primitives (id) VALUES (10);
INSERT INTO user_1_primitives (id) VALUES (11);
INSERT INTO user_1_primitives (id) VALUES (12);
INSERT INTO user_1_primitives (id) VALUES (13);
INSERT INTO user_1_primitives (id) VALUES (14);
INSERT INTO user_1_primitives (id) VALUES (15);
INSERT INTO user_1_primitives (id) VALUES (72);
INSERT INTO user_1_primitives (id) VALUES (77);


--
-- Data for Name: user_1_reads; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_reads (func, var) VALUES (76, 65);
INSERT INTO user_1_reads (func, var) VALUES (78, 67);
INSERT INTO user_1_reads (func, var) VALUES (78, 68);
INSERT INTO user_1_reads (func, var) VALUES (78, 69);
INSERT INTO user_1_reads (func, var) VALUES (79, 67);
INSERT INTO user_1_reads (func, var) VALUES (79, 68);
INSERT INTO user_1_reads (func, var) VALUES (79, 69);
INSERT INTO user_1_reads (func, var) VALUES (80, 67);
INSERT INTO user_1_reads (func, var) VALUES (80, 68);
INSERT INTO user_1_reads (func, var) VALUES (80, 69);
INSERT INTO user_1_reads (func, var) VALUES (84, 60);
INSERT INTO user_1_reads (func, var) VALUES (91, 90);
INSERT INTO user_1_reads (func, var) VALUES (93, 92);


--
-- Data for Name: user_1_types; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_types (id, name) VALUES (10, 'char');
INSERT INTO user_1_types (id, name) VALUES (11, 'short');
INSERT INTO user_1_types (id, name) VALUES (12, 'long');
INSERT INTO user_1_types (id, name) VALUES (13, 'string');
INSERT INTO user_1_types (id, name) VALUES (14, 'object');
INSERT INTO user_1_types (id, name) VALUES (15, 'int');
INSERT INTO user_1_types (id, name) VALUES (56, 'Bool');
INSERT INTO user_1_types (id, name) VALUES (59, 'File');
INSERT INTO user_1_types (id, name) VALUES (63, 'FooBar');
INSERT INTO user_1_types (id, name) VALUES (66, 'Vector3D');
INSERT INTO user_1_types (id, name) VALUES (72, 'void');
INSERT INTO user_1_types (id, name) VALUES (77, 'float');
INSERT INTO user_1_types (id, name) VALUES (82, 'FooBarBase');
INSERT INTO user_1_types (id, name) VALUES (85, 'AParentClass');
INSERT INTO user_1_types (id, name) VALUES (86, 'AChildClass');
INSERT INTO user_1_types (id, name) VALUES (87, 'AGrandparentClass');
INSERT INTO user_1_types (id, name) VALUES (95, 'AnotherClass');
INSERT INTO user_1_types (id, name) VALUES (96, 'UnorderedAlphabet');
INSERT INTO user_1_types (id, name) VALUES (123, 'ClassWithFunctionsCallingEachOther');


--
-- Data for Name: user_1_values; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_values (id, name) VALUES (2, 'testField');
INSERT INTO user_1_values (id, name) VALUES (5, 'testMethod');
INSERT INTO user_1_values (id, name) VALUES (18, 'True');
INSERT INTO user_1_values (id, name) VALUES (19, 'False');
INSERT INTO user_1_values (id, name) VALUES (22, 'True');
INSERT INTO user_1_values (id, name) VALUES (23, 'False');
INSERT INTO user_1_values (id, name) VALUES (25, 'testField');
INSERT INTO user_1_values (id, name) VALUES (26, 'testMethod');
INSERT INTO user_1_values (id, name) VALUES (27, 'doSth');
INSERT INTO user_1_values (id, name) VALUES (39, 'true');
INSERT INTO user_1_values (id, name) VALUES (40, 'false');
INSERT INTO user_1_values (id, name) VALUES (42, 'A');
INSERT INTO user_1_values (id, name) VALUES (43, 'B');
INSERT INTO user_1_values (id, name) VALUES (44, 'C');
INSERT INTO user_1_values (id, name) VALUES (45, 'D');
INSERT INTO user_1_values (id, name) VALUES (46, 'E');
INSERT INTO user_1_values (id, name) VALUES (47, 'F');
INSERT INTO user_1_values (id, name) VALUES (48, 'G');
INSERT INTO user_1_values (id, name) VALUES (49, 'H');
INSERT INTO user_1_values (id, name) VALUES (50, 'J');
INSERT INTO user_1_values (id, name) VALUES (51, 'K');
INSERT INTO user_1_values (id, name) VALUES (52, 'content');
INSERT INTO user_1_values (id, name) VALUES (53, 'name');
INSERT INTO user_1_values (id, name) VALUES (57, 'True');
INSERT INTO user_1_values (id, name) VALUES (58, 'False');
INSERT INTO user_1_values (id, name) VALUES (60, 'content');
INSERT INTO user_1_values (id, name) VALUES (61, 'name');
INSERT INTO user_1_values (id, name) VALUES (62, 'size');
INSERT INTO user_1_values (id, name) VALUES (64, 'foo_');
INSERT INTO user_1_values (id, name) VALUES (65, 'bar_');
INSERT INTO user_1_values (id, name) VALUES (67, 'x');
INSERT INTO user_1_values (id, name) VALUES (68, 'y');
INSERT INTO user_1_values (id, name) VALUES (69, 'z');
INSERT INTO user_1_values (id, name) VALUES (70, 'open');
INSERT INTO user_1_values (id, name) VALUES (73, 'close');
INSERT INTO user_1_values (id, name) VALUES (74, 'maxSize');
INSERT INTO user_1_values (id, name) VALUES (75, 'foo');
INSERT INTO user_1_values (id, name) VALUES (76, 'getBar');
INSERT INTO user_1_values (id, name) VALUES (78, 'length');
INSERT INTO user_1_values (id, name) VALUES (79, 'dot');
INSERT INTO user_1_values (id, name) VALUES (80, 'cross');
INSERT INTO user_1_values (id, name) VALUES (81, 'reset');
INSERT INTO user_1_values (id, name) VALUES (83, 'fooBase');
INSERT INTO user_1_values (id, name) VALUES (84, 'read');
INSERT INTO user_1_values (id, name) VALUES (88, 'testField');
INSERT INTO user_1_values (id, name) VALUES (89, 'modifyTestField');
INSERT INTO user_1_values (id, name) VALUES (90, 'anotherField');
INSERT INTO user_1_values (id, name) VALUES (91, 'readAnotherField');
INSERT INTO user_1_values (id, name) VALUES (92, 'a');
INSERT INTO user_1_values (id, name) VALUES (93, 'getA');
INSERT INTO user_1_values (id, name) VALUES (97, 'Q');
INSERT INTO user_1_values (id, name) VALUES (98, 'W');
INSERT INTO user_1_values (id, name) VALUES (99, 'E');
INSERT INTO user_1_values (id, name) VALUES (100, 'R');
INSERT INTO user_1_values (id, name) VALUES (101, 'T');
INSERT INTO user_1_values (id, name) VALUES (102, 'Y');
INSERT INTO user_1_values (id, name) VALUES (103, 'U');
INSERT INTO user_1_values (id, name) VALUES (104, 'I');
INSERT INTO user_1_values (id, name) VALUES (105, 'O');
INSERT INTO user_1_values (id, name) VALUES (106, 'P');
INSERT INTO user_1_values (id, name) VALUES (107, 'A');
INSERT INTO user_1_values (id, name) VALUES (108, 'S');
INSERT INTO user_1_values (id, name) VALUES (109, 'D');
INSERT INTO user_1_values (id, name) VALUES (110, 'F');
INSERT INTO user_1_values (id, name) VALUES (111, 'G');
INSERT INTO user_1_values (id, name) VALUES (112, 'H');
INSERT INTO user_1_values (id, name) VALUES (113, 'J');
INSERT INTO user_1_values (id, name) VALUES (114, 'K');
INSERT INTO user_1_values (id, name) VALUES (115, 'L');
INSERT INTO user_1_values (id, name) VALUES (116, 'Z');
INSERT INTO user_1_values (id, name) VALUES (117, 'X');
INSERT INTO user_1_values (id, name) VALUES (118, 'C');
INSERT INTO user_1_values (id, name) VALUES (119, 'V');
INSERT INTO user_1_values (id, name) VALUES (120, 'B');
INSERT INTO user_1_values (id, name) VALUES (121, 'N');
INSERT INTO user_1_values (id, name) VALUES (122, 'M');
INSERT INTO user_1_values (id, name) VALUES (124, 'func');
INSERT INTO user_1_values (id, name) VALUES (126, 'fibb');
INSERT INTO user_1_values (id, name) VALUES (127, 'abc');
INSERT INTO user_1_values (id, name) VALUES (128, 'normalize');
INSERT INTO user_1_values (id, name) VALUES (130, 'sub');
INSERT INTO user_1_values (id, name) VALUES (135, 'add');
INSERT INTO user_1_values (id, name) VALUES (136, 'div');
INSERT INTO user_1_values (id, name) VALUES (137, 'mul');
INSERT INTO user_1_values (id, name) VALUES (138, 'def');


--
-- Data for Name: user_1_writes; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO user_1_writes (func, var) VALUES (75, 64);
INSERT INTO user_1_writes (func, var) VALUES (80, 67);
INSERT INTO user_1_writes (func, var) VALUES (80, 68);
INSERT INTO user_1_writes (func, var) VALUES (80, 69);
INSERT INTO user_1_writes (func, var) VALUES (81, 67);
INSERT INTO user_1_writes (func, var) VALUES (81, 68);
INSERT INTO user_1_writes (func, var) VALUES (81, 69);
INSERT INTO user_1_writes (func, var) VALUES (70, 60);
INSERT INTO user_1_writes (func, var) VALUES (70, 61);
INSERT INTO user_1_writes (func, var) VALUES (70, 62);
INSERT INTO user_1_writes (func, var) VALUES (73, 62);
INSERT INTO user_1_writes (func, var) VALUES (73, 60);
INSERT INTO user_1_writes (func, var) VALUES (89, 88);


--
-- Data for Name: users; Type: TABLE DATA; Schema: pn347193; Owner: -
--

INSERT INTO users (id, username, password) VALUES (1, 'test', 'test');


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: pn347193; Owner: -
--

SELECT pg_catalog.setval('users_id_seq', 7, true);


--
-- Name: user_1_arguments_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_arguments
    ADD CONSTRAINT user_1_arguments_pkey PRIMARY KEY (func, index);


--
-- Name: user_1_calls_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_calls
    ADD CONSTRAINT user_1_calls_pkey PRIMARY KEY (caller, callee);


--
-- Name: user_1_classes_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_classes
    ADD CONSTRAINT user_1_classes_pkey PRIMARY KEY (id);


--
-- Name: user_1_declarations_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_declarations
    ADD CONSTRAINT user_1_declarations_pkey PRIMARY KEY (id);


--
-- Name: user_1_enumerations_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_enumerations
    ADD CONSTRAINT user_1_enumerations_pkey PRIMARY KEY (id);


--
-- Name: user_1_enumerators_name_enum_key; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_enumerators
    ADD CONSTRAINT user_1_enumerators_name_enum_key UNIQUE (name, enum);


--
-- Name: user_1_enumerators_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_enumerators
    ADD CONSTRAINT user_1_enumerators_pkey PRIMARY KEY (id);


--
-- Name: user_1_fields_name_class_key; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_fields
    ADD CONSTRAINT user_1_fields_name_class_key UNIQUE (name, class);


--
-- Name: user_1_fields_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_fields
    ADD CONSTRAINT user_1_fields_pkey PRIMARY KEY (id);


--
-- Name: user_1_inherits_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_inherits
    ADD CONSTRAINT user_1_inherits_pkey PRIMARY KEY (parent, child);


--
-- Name: user_1_methods_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_methods
    ADD CONSTRAINT user_1_methods_pkey PRIMARY KEY (id);


--
-- Name: user_1_primitives_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_primitives
    ADD CONSTRAINT user_1_primitives_pkey PRIMARY KEY (id);


--
-- Name: user_1_reads_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_reads
    ADD CONSTRAINT user_1_reads_pkey PRIMARY KEY (func, var);


--
-- Name: user_1_types_name_key; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_types
    ADD CONSTRAINT user_1_types_name_key UNIQUE (name);


--
-- Name: user_1_types_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_types
    ADD CONSTRAINT user_1_types_pkey PRIMARY KEY (id);


--
-- Name: user_1_values_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_values
    ADD CONSTRAINT user_1_values_pkey PRIMARY KEY (id, name);


--
-- Name: user_1_writes_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY user_1_writes
    ADD CONSTRAINT user_1_writes_pkey PRIMARY KEY (func, var);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users_username_key; Type: CONSTRAINT; Schema: pn347193; Owner: -; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: usercreated; Type: TRIGGER; Schema: pn347193; Owner: -
--

CREATE TRIGGER usercreated AFTER INSERT ON users FOR EACH ROW EXECUTE PROCEDURE createuserschema();


--
-- Name: userdeleted; Type: TRIGGER; Schema: pn347193; Owner: -
--

CREATE TRIGGER userdeleted AFTER DELETE ON users FOR EACH ROW EXECUTE PROCEDURE dropuserschema();


--
-- Name: user_1_arguments_func_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_arguments
    ADD CONSTRAINT user_1_arguments_func_fkey FOREIGN KEY (func) REFERENCES user_1_methods(id) ON DELETE CASCADE;


--
-- Name: user_1_arguments_type_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_arguments
    ADD CONSTRAINT user_1_arguments_type_fkey FOREIGN KEY (type) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_calls_callee_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_calls
    ADD CONSTRAINT user_1_calls_callee_fkey FOREIGN KEY (callee) REFERENCES user_1_methods(id) ON DELETE CASCADE;


--
-- Name: user_1_calls_caller_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_calls
    ADD CONSTRAINT user_1_calls_caller_fkey FOREIGN KEY (caller) REFERENCES user_1_methods(id) ON DELETE CASCADE;


--
-- Name: user_1_classes_id_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_classes
    ADD CONSTRAINT user_1_classes_id_fkey FOREIGN KEY (id) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_enumerations_id_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_enumerations
    ADD CONSTRAINT user_1_enumerations_id_fkey FOREIGN KEY (id) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_enumerators_enum_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_enumerators
    ADD CONSTRAINT user_1_enumerators_enum_fkey FOREIGN KEY (enum) REFERENCES user_1_enumerations(id) ON DELETE CASCADE;


--
-- Name: user_1_enumerators_id_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_enumerators
    ADD CONSTRAINT user_1_enumerators_id_fkey FOREIGN KEY (id, name) REFERENCES user_1_values(id, name) ON DELETE CASCADE;


--
-- Name: user_1_fields_class_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_fields
    ADD CONSTRAINT user_1_fields_class_fkey FOREIGN KEY (class) REFERENCES user_1_classes(id) ON DELETE CASCADE;


--
-- Name: user_1_fields_id_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_fields
    ADD CONSTRAINT user_1_fields_id_fkey FOREIGN KEY (id, name) REFERENCES user_1_values(id, name) ON DELETE CASCADE;


--
-- Name: user_1_fields_type_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_fields
    ADD CONSTRAINT user_1_fields_type_fkey FOREIGN KEY (type) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_inherits_child_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_inherits
    ADD CONSTRAINT user_1_inherits_child_fkey FOREIGN KEY (child) REFERENCES user_1_classes(id) ON DELETE CASCADE;


--
-- Name: user_1_inherits_parent_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_inherits
    ADD CONSTRAINT user_1_inherits_parent_fkey FOREIGN KEY (parent) REFERENCES user_1_classes(id) ON DELETE CASCADE;


--
-- Name: user_1_methods_class_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_methods
    ADD CONSTRAINT user_1_methods_class_fkey FOREIGN KEY (class) REFERENCES user_1_classes(id) ON DELETE CASCADE;


--
-- Name: user_1_methods_id_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_methods
    ADD CONSTRAINT user_1_methods_id_fkey FOREIGN KEY (id, name) REFERENCES user_1_values(id, name) ON DELETE CASCADE;


--
-- Name: user_1_methods_returntype_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_methods
    ADD CONSTRAINT user_1_methods_returntype_fkey FOREIGN KEY (returntype) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_primitives_id_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_primitives
    ADD CONSTRAINT user_1_primitives_id_fkey FOREIGN KEY (id) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_reads_func_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_reads
    ADD CONSTRAINT user_1_reads_func_fkey FOREIGN KEY (func) REFERENCES user_1_methods(id) ON DELETE CASCADE;


--
-- Name: user_1_reads_var_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_reads
    ADD CONSTRAINT user_1_reads_var_fkey FOREIGN KEY (var) REFERENCES user_1_fields(id) ON DELETE CASCADE;


--
-- Name: user_1_types_id_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_types
    ADD CONSTRAINT user_1_types_id_fkey FOREIGN KEY (id) REFERENCES user_1_declarations(id) ON DELETE CASCADE;


--
-- Name: user_1_values_id_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_values
    ADD CONSTRAINT user_1_values_id_fkey FOREIGN KEY (id) REFERENCES user_1_declarations(id) ON DELETE CASCADE;


--
-- Name: user_1_writes_func_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_writes
    ADD CONSTRAINT user_1_writes_func_fkey FOREIGN KEY (func) REFERENCES user_1_methods(id) ON DELETE CASCADE;


--
-- Name: user_1_writes_var_fkey; Type: FK CONSTRAINT; Schema: pn347193; Owner: -
--

ALTER TABLE ONLY user_1_writes
    ADD CONSTRAINT user_1_writes_var_fkey FOREIGN KEY (var) REFERENCES user_1_fields(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

