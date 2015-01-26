-- The $ symbol is a placeholder for schema name.
-- I wanted to use schemas here, but I don't have permissions
-- to create them on labdb. So I am prefixing table names.

-- CREATE SCHEMA $;

create table $_Files (
  id int primary key,
  path varchar(1024) unique not null
);

create table $_Declarations (
  id int primary key,
  file int references $_Files(id)
);

create table $_Types (
  id int primary key references $_Declarations(id),
  name varchar(1024) unique not null
);

create table $_Values (
  id int references $_Declarations(id),
  name varchar(1024) not null,
  primary key (id, name)
);

-- Types.

create table $_Primitives (
  id int primary key references $_Types(id)
);

create table $_Classes (
  id int primary key references $_Types(id),
  isStruct bool not null
);

create table $_Enumerations (
  id int primary key references $_Types(id)
);

-- Values.

create table $_Fields (
  id int primary key,
  name varchar(1024) not null,
  class int not null references $_Classes(id),
  static bool not null,
  foreign key (id, name) references $_Values(id, name),
  unique (name, class)
);

create table $_Enumerators (
  id int primary key,
  name varchar(1024) not null,
  enum int not null references $_Enumerations(id),
  foreign key (id, name) references $_Values(id, name),
  unique (name, enum)
);

create table $_Methods (
  id int primary key,
  name varchar(1024) not null,
  class int references $_Classes(id),
  static bool not null,
  returnType int not null references $_Types(id),
  foreign key (id, name) references $_Values(id, name)
);

create table $_Arguments (
  func int references $_Methods(id),
  index int not null,
  type int not null references $_Types(id),
  primary key (func, index)
);

-- Relations.

create table $_Inherits (
  parent int references $_Classes(id),
  child int references $_Classes(id),
  primary key (parent, child)
);

create table $_Calls (
  caller int references $_Methods(id),
  callee int references $_Methods(id),
  primary key (caller, callee)
);

create table $_Reads (
  func int references $_Methods(id),
  var int references $_Fields(id),
  primary key (func, var)
);

create table $_Writes (
  func int references $_Methods(id),
  var int references $_Fields(id),
  primary key (func, var)
);
