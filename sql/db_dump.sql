--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = public, pg_catalog;

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
-- PostgreSQL database dump complete
--

