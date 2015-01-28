--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: createuserschema(); Type: FUNCTION; Schema: public; Owner: yesod
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


ALTER FUNCTION public.createuserschema() OWNER TO yesod;

--
-- Name: dropuserschema(); Type: FUNCTION; Schema: public; Owner: yesod
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


ALTER FUNCTION public.dropuserschema() OWNER TO yesod;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: user_1_arguments; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_arguments (
    func integer NOT NULL,
    index integer NOT NULL,
    type integer NOT NULL
);


ALTER TABLE user_1_arguments OWNER TO yesod;

--
-- Name: user_1_calls; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_calls (
    caller integer NOT NULL,
    callee integer NOT NULL
);


ALTER TABLE user_1_calls OWNER TO yesod;

--
-- Name: user_1_classes; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_classes (
    id integer NOT NULL,
    isstruct boolean NOT NULL
);


ALTER TABLE user_1_classes OWNER TO yesod;

--
-- Name: user_1_declarations; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_declarations (
    id integer NOT NULL,
    file character varying(1024)
);


ALTER TABLE user_1_declarations OWNER TO yesod;

--
-- Name: user_1_declarations_id_seq; Type: SEQUENCE; Schema: public; Owner: yesod
--

CREATE SEQUENCE user_1_declarations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE user_1_declarations_id_seq OWNER TO yesod;

--
-- Name: user_1_declarations_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: yesod
--

ALTER SEQUENCE user_1_declarations_id_seq OWNED BY user_1_declarations.id;


--
-- Name: user_1_enumerations; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_enumerations (
    id integer NOT NULL
);


ALTER TABLE user_1_enumerations OWNER TO yesod;

--
-- Name: user_1_enumerators; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_enumerators (
    id integer NOT NULL,
    name character varying(1024) NOT NULL,
    enum integer NOT NULL
);


ALTER TABLE user_1_enumerators OWNER TO yesod;

--
-- Name: user_1_fields; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_fields (
    id integer NOT NULL,
    name character varying(1024) NOT NULL,
    class integer NOT NULL,
    static boolean NOT NULL,
    type integer NOT NULL
);


ALTER TABLE user_1_fields OWNER TO yesod;

--
-- Name: user_1_inherits; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_inherits (
    parent integer NOT NULL,
    child integer NOT NULL
);


ALTER TABLE user_1_inherits OWNER TO yesod;

--
-- Name: user_1_methods; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_methods (
    id integer NOT NULL,
    name character varying(1024) NOT NULL,
    class integer,
    static boolean NOT NULL,
    returntype integer NOT NULL
);


ALTER TABLE user_1_methods OWNER TO yesod;

--
-- Name: user_1_primitives; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_primitives (
    id integer NOT NULL
);


ALTER TABLE user_1_primitives OWNER TO yesod;

--
-- Name: user_1_reads; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_reads (
    func integer NOT NULL,
    var integer NOT NULL
);


ALTER TABLE user_1_reads OWNER TO yesod;

--
-- Name: user_1_types; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_types (
    id integer NOT NULL,
    name character varying(1024) NOT NULL
);


ALTER TABLE user_1_types OWNER TO yesod;

--
-- Name: user_1_values; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_values (
    id integer NOT NULL,
    name character varying(1024) NOT NULL
);


ALTER TABLE user_1_values OWNER TO yesod;

--
-- Name: user_1_writes; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE user_1_writes (
    func integer NOT NULL,
    var integer NOT NULL
);


ALTER TABLE user_1_writes OWNER TO yesod;

--
-- Name: users; Type: TABLE; Schema: public; Owner: yesod; Tablespace: 
--

CREATE TABLE users (
    id integer NOT NULL,
    username character varying(256),
    password character varying(256)
);


ALTER TABLE users OWNER TO yesod;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: yesod
--

CREATE SEQUENCE users_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE users_id_seq OWNER TO yesod;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: yesod
--

ALTER SEQUENCE users_id_seq OWNED BY users.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_declarations ALTER COLUMN id SET DEFAULT nextval('user_1_declarations_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY users ALTER COLUMN id SET DEFAULT nextval('users_id_seq'::regclass);


--
-- Data for Name: user_1_arguments; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_arguments (func, index, type) FROM stdin;
70	0	13
75	0	15
75	1	10
75	2	66
79	0	66
79	1	66
80	0	66
80	1	66
83	0	13
124	0	15
124	1	15
126	0	15
130	0	66
135	0	66
136	0	15
137	0	15
\.


--
-- Data for Name: user_1_calls; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_calls (caller, callee) FROM stdin;
76	75
75	83
124	124
126	126
124	126
127	124
78	79
128	78
128	137
127	138
138	127
\.


--
-- Data for Name: user_1_classes; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_classes (id, isstruct) FROM stdin;
59	f
63	f
66	t
82	f
85	f
86	f
87	f
95	f
123	f
\.


--
-- Data for Name: user_1_declarations; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_declarations (id, file) FROM stdin;
2	Test.java
5	Test.java
10	\N
11	\N
12	\N
13	\N
14	\N
15	\N
18	Bool.java
19	Bool.java
22	Bool.java
23	Bool.java
25	\N
26	\N
27	\N
39	Bool.java
40	Bool.java
42	\N
43	\N
44	\N
45	\N
46	\N
47	\N
48	\N
49	\N
50	\N
51	\N
52	\N
53	\N
56	Bool.java
57	Bool.java
58	Bool.java
59	File.java
60	\N
61	\N
62	\N
63	FooBar.java
64	\N
65	\N
66	Vector3D.java
67	\N
68	\N
69	\N
70	\N
72	\N
73	\N
74	\N
75	\N
76	\N
77	\N
78	\N
79	\N
80	\N
81	\N
82	FooBarBase.java
83	\N
84	\N
85	\N
86	\N
87	\N
88	\N
89	\N
90	\N
91	\N
92	\N
93	\N
95	\N
96	UnorderedAlphabet.java
97	UnorderedAlphabet.java
98	UnorderedAlphabet.java
99	UnorderedAlphabet.java
100	UnorderedAlphabet.java
101	UnorderedAlphabet.java
102	UnorderedAlphabet.java
103	UnorderedAlphabet.java
104	UnorderedAlphabet.java
105	UnorderedAlphabet.java
106	UnorderedAlphabet.java
107	UnorderedAlphabet.java
108	UnorderedAlphabet.java
109	UnorderedAlphabet.java
110	UnorderedAlphabet.java
111	UnorderedAlphabet.java
112	UnorderedAlphabet.java
113	UnorderedAlphabet.java
114	UnorderedAlphabet.java
115	UnorderedAlphabet.java
116	UnorderedAlphabet.java
117	UnorderedAlphabet.java
118	UnorderedAlphabet.java
119	UnorderedAlphabet.java
120	UnorderedAlphabet.java
121	UnorderedAlphabet.java
122	UnorderedAlphabet.java
123	src/some/file/in/some/language.txt
124	\N
126	\N
127	\N
128	\N
130	\N
135	\N
136	\N
137	\N
138	\N
\.


--
-- Name: user_1_declarations_id_seq; Type: SEQUENCE SET; Schema: public; Owner: yesod
--

SELECT pg_catalog.setval('user_1_declarations_id_seq', 138, true);


--
-- Data for Name: user_1_enumerations; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_enumerations (id) FROM stdin;
56
96
\.


--
-- Data for Name: user_1_enumerators; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_enumerators (id, name, enum) FROM stdin;
57	True	56
58	False	56
97	Q	96
98	W	96
99	E	96
100	R	96
101	T	96
102	Y	96
103	U	96
104	I	96
105	O	96
106	P	96
107	A	96
108	S	96
109	D	96
110	F	96
111	G	96
112	H	96
113	J	96
114	K	96
115	L	96
116	Z	96
117	X	96
118	C	96
119	V	96
120	B	96
121	N	96
122	M	96
\.


--
-- Data for Name: user_1_fields; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_fields (id, name, class, static, type) FROM stdin;
60	content	59	f	13
61	name	59	f	13
62	size	59	f	15
64	foo_	63	t	59
65	bar_	63	f	63
67	x	66	f	15
68	y	66	f	15
69	z	66	f	15
88	testField	86	f	15
90	anotherField	86	f	13
92	a	87	f	15
\.


--
-- Data for Name: user_1_inherits; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_inherits (parent, child) FROM stdin;
63	82
86	85
85	87
86	59
95	86
\.


--
-- Data for Name: user_1_methods; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_methods (id, name, class, static, returntype) FROM stdin;
70	open	59	f	56
73	close	59	f	72
74	maxSize	59	t	15
75	foo	63	t	11
76	getBar	63	f	63
78	length	66	f	77
79	dot	66	t	77
80	cross	66	t	66
81	reset	66	f	72
83	fooBase	82	f	15
84	read	59	f	13
89	modifyTestField	86	f	72
91	readAnotherField	86	f	13
93	getA	87	f	15
124	func	123	t	63
126	fibb	123	t	15
127	abc	123	f	72
128	normalize	66	f	66
130	sub	66	f	66
135	add	66	f	66
136	div	66	f	66
137	mul	66	f	66
138	def	123	t	72
\.


--
-- Data for Name: user_1_primitives; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_primitives (id) FROM stdin;
10
11
12
13
14
15
72
77
\.


--
-- Data for Name: user_1_reads; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_reads (func, var) FROM stdin;
76	65
78	67
78	68
78	69
79	67
79	68
79	69
80	67
80	68
80	69
84	60
91	90
93	92
\.


--
-- Data for Name: user_1_types; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_types (id, name) FROM stdin;
10	char
11	short
12	long
13	string
14	object
15	int
56	Bool
59	File
63	FooBar
66	Vector3D
72	void
77	float
82	FooBarBase
85	AParentClass
86	AChildClass
87	AGrandparentClass
95	AnotherClass
96	UnorderedAlphabet
123	ClassWithFunctionsCallingEachOther
\.


--
-- Data for Name: user_1_values; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_values (id, name) FROM stdin;
2	testField
5	testMethod
18	True
19	False
22	True
23	False
25	testField
26	testMethod
27	doSth
39	true
40	false
42	A
43	B
44	C
45	D
46	E
47	F
48	G
49	H
50	J
51	K
52	content
53	name
57	True
58	False
60	content
61	name
62	size
64	foo_
65	bar_
67	x
68	y
69	z
70	open
73	close
74	maxSize
75	foo
76	getBar
78	length
79	dot
80	cross
81	reset
83	fooBase
84	read
88	testField
89	modifyTestField
90	anotherField
91	readAnotherField
92	a
93	getA
97	Q
98	W
99	E
100	R
101	T
102	Y
103	U
104	I
105	O
106	P
107	A
108	S
109	D
110	F
111	G
112	H
113	J
114	K
115	L
116	Z
117	X
118	C
119	V
120	B
121	N
122	M
124	func
126	fibb
127	abc
128	normalize
130	sub
135	add
136	div
137	mul
138	def
\.


--
-- Data for Name: user_1_writes; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY user_1_writes (func, var) FROM stdin;
75	64
80	67
80	68
80	69
81	67
81	68
81	69
70	60
70	61
70	62
73	62
73	60
89	88
\.


--
-- Data for Name: users; Type: TABLE DATA; Schema: public; Owner: yesod
--

COPY users (id, username, password) FROM stdin;
1	test	test
\.


--
-- Name: users_id_seq; Type: SEQUENCE SET; Schema: public; Owner: yesod
--

SELECT pg_catalog.setval('users_id_seq', 7, true);


--
-- Name: user_1_arguments_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_arguments
    ADD CONSTRAINT user_1_arguments_pkey PRIMARY KEY (func, index);


--
-- Name: user_1_calls_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_calls
    ADD CONSTRAINT user_1_calls_pkey PRIMARY KEY (caller, callee);


--
-- Name: user_1_classes_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_classes
    ADD CONSTRAINT user_1_classes_pkey PRIMARY KEY (id);


--
-- Name: user_1_declarations_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_declarations
    ADD CONSTRAINT user_1_declarations_pkey PRIMARY KEY (id);


--
-- Name: user_1_enumerations_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_enumerations
    ADD CONSTRAINT user_1_enumerations_pkey PRIMARY KEY (id);


--
-- Name: user_1_enumerators_name_enum_key; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_enumerators
    ADD CONSTRAINT user_1_enumerators_name_enum_key UNIQUE (name, enum);


--
-- Name: user_1_enumerators_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_enumerators
    ADD CONSTRAINT user_1_enumerators_pkey PRIMARY KEY (id);


--
-- Name: user_1_fields_name_class_key; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_fields
    ADD CONSTRAINT user_1_fields_name_class_key UNIQUE (name, class);


--
-- Name: user_1_fields_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_fields
    ADD CONSTRAINT user_1_fields_pkey PRIMARY KEY (id);


--
-- Name: user_1_inherits_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_inherits
    ADD CONSTRAINT user_1_inherits_pkey PRIMARY KEY (parent, child);


--
-- Name: user_1_methods_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_methods
    ADD CONSTRAINT user_1_methods_pkey PRIMARY KEY (id);


--
-- Name: user_1_primitives_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_primitives
    ADD CONSTRAINT user_1_primitives_pkey PRIMARY KEY (id);


--
-- Name: user_1_reads_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_reads
    ADD CONSTRAINT user_1_reads_pkey PRIMARY KEY (func, var);


--
-- Name: user_1_types_name_key; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_types
    ADD CONSTRAINT user_1_types_name_key UNIQUE (name);


--
-- Name: user_1_types_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_types
    ADD CONSTRAINT user_1_types_pkey PRIMARY KEY (id);


--
-- Name: user_1_values_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_values
    ADD CONSTRAINT user_1_values_pkey PRIMARY KEY (id, name);


--
-- Name: user_1_writes_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY user_1_writes
    ADD CONSTRAINT user_1_writes_pkey PRIMARY KEY (func, var);


--
-- Name: users_pkey; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: users_username_key; Type: CONSTRAINT; Schema: public; Owner: yesod; Tablespace: 
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_username_key UNIQUE (username);


--
-- Name: usercreated; Type: TRIGGER; Schema: public; Owner: yesod
--

CREATE TRIGGER usercreated AFTER INSERT ON users FOR EACH ROW EXECUTE PROCEDURE createuserschema();


--
-- Name: userdeleted; Type: TRIGGER; Schema: public; Owner: yesod
--

CREATE TRIGGER userdeleted AFTER DELETE ON users FOR EACH ROW EXECUTE PROCEDURE dropuserschema();


--
-- Name: user_1_arguments_func_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_arguments
    ADD CONSTRAINT user_1_arguments_func_fkey FOREIGN KEY (func) REFERENCES user_1_methods(id) ON DELETE CASCADE;


--
-- Name: user_1_arguments_type_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_arguments
    ADD CONSTRAINT user_1_arguments_type_fkey FOREIGN KEY (type) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_calls_callee_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_calls
    ADD CONSTRAINT user_1_calls_callee_fkey FOREIGN KEY (callee) REFERENCES user_1_methods(id) ON DELETE CASCADE;


--
-- Name: user_1_calls_caller_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_calls
    ADD CONSTRAINT user_1_calls_caller_fkey FOREIGN KEY (caller) REFERENCES user_1_methods(id) ON DELETE CASCADE;


--
-- Name: user_1_classes_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_classes
    ADD CONSTRAINT user_1_classes_id_fkey FOREIGN KEY (id) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_enumerations_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_enumerations
    ADD CONSTRAINT user_1_enumerations_id_fkey FOREIGN KEY (id) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_enumerators_enum_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_enumerators
    ADD CONSTRAINT user_1_enumerators_enum_fkey FOREIGN KEY (enum) REFERENCES user_1_enumerations(id) ON DELETE CASCADE;


--
-- Name: user_1_enumerators_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_enumerators
    ADD CONSTRAINT user_1_enumerators_id_fkey FOREIGN KEY (id, name) REFERENCES user_1_values(id, name) ON DELETE CASCADE;


--
-- Name: user_1_fields_class_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_fields
    ADD CONSTRAINT user_1_fields_class_fkey FOREIGN KEY (class) REFERENCES user_1_classes(id) ON DELETE CASCADE;


--
-- Name: user_1_fields_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_fields
    ADD CONSTRAINT user_1_fields_id_fkey FOREIGN KEY (id, name) REFERENCES user_1_values(id, name) ON DELETE CASCADE;


--
-- Name: user_1_fields_type_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_fields
    ADD CONSTRAINT user_1_fields_type_fkey FOREIGN KEY (type) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_inherits_child_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_inherits
    ADD CONSTRAINT user_1_inherits_child_fkey FOREIGN KEY (child) REFERENCES user_1_classes(id) ON DELETE CASCADE;


--
-- Name: user_1_inherits_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_inherits
    ADD CONSTRAINT user_1_inherits_parent_fkey FOREIGN KEY (parent) REFERENCES user_1_classes(id) ON DELETE CASCADE;


--
-- Name: user_1_methods_class_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_methods
    ADD CONSTRAINT user_1_methods_class_fkey FOREIGN KEY (class) REFERENCES user_1_classes(id) ON DELETE CASCADE;


--
-- Name: user_1_methods_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_methods
    ADD CONSTRAINT user_1_methods_id_fkey FOREIGN KEY (id, name) REFERENCES user_1_values(id, name) ON DELETE CASCADE;


--
-- Name: user_1_methods_returntype_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_methods
    ADD CONSTRAINT user_1_methods_returntype_fkey FOREIGN KEY (returntype) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_primitives_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_primitives
    ADD CONSTRAINT user_1_primitives_id_fkey FOREIGN KEY (id) REFERENCES user_1_types(id) ON DELETE CASCADE;


--
-- Name: user_1_reads_func_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_reads
    ADD CONSTRAINT user_1_reads_func_fkey FOREIGN KEY (func) REFERENCES user_1_methods(id) ON DELETE CASCADE;


--
-- Name: user_1_reads_var_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_reads
    ADD CONSTRAINT user_1_reads_var_fkey FOREIGN KEY (var) REFERENCES user_1_fields(id) ON DELETE CASCADE;


--
-- Name: user_1_types_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_types
    ADD CONSTRAINT user_1_types_id_fkey FOREIGN KEY (id) REFERENCES user_1_declarations(id) ON DELETE CASCADE;


--
-- Name: user_1_values_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_values
    ADD CONSTRAINT user_1_values_id_fkey FOREIGN KEY (id) REFERENCES user_1_declarations(id) ON DELETE CASCADE;


--
-- Name: user_1_writes_func_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_writes
    ADD CONSTRAINT user_1_writes_func_fkey FOREIGN KEY (func) REFERENCES user_1_methods(id) ON DELETE CASCADE;


--
-- Name: user_1_writes_var_fkey; Type: FK CONSTRAINT; Schema: public; Owner: yesod
--

ALTER TABLE ONLY user_1_writes
    ADD CONSTRAINT user_1_writes_var_fkey FOREIGN KEY (var) REFERENCES user_1_fields(id) ON DELETE CASCADE;


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

