-- The $ symbol is a placeholder for schema name.
-- I wanted to use schemas here, but I don't have permissions
-- to create them on labdb. So I am prefixing table names.

-- CREATE SCHEMA $;

create table $.Declarations (
  id serial primary key,
  file varchar(1024)
);

create table $.Types (
  id int primary key references $.Declarations(id) on delete cascade,
  name varchar(1024) unique not null
);

create table $.Values (
  id int references $.Declarations(id) on delete cascade,
  name varchar(1024) not null,
  primary key (id, name)
);

-- Types.

create table $.Primitives (
  id int primary key references $.Types(id) on delete cascade
);

create table $.Classes (
  id int primary key references $.Types(id) on delete cascade,
  isStruct bool not null
);

create table $.Enumerations (
  id int primary key references $.Types(id) on delete cascade
);

-- Values.

create table $.Fields (
  id int primary key,
  name varchar(1024) not null,
  class int not null references $.Classes(id) on delete cascade,
  static bool not null,
  type int not null references $.Types(id) on delete cascade,
  foreign key (id, name) references $.Values(id, name) on delete cascade,
  unique (name, class)
);

create table $.Enumerators (
  id int primary key,
  name varchar(1024) not null,
  enum int not null references $.Enumerations(id) on delete cascade,
  foreign key (id, name) references $.Values(id, name) on delete cascade,
  unique (name, enum)
);

create table $.Methods (
  id int primary key,
  name varchar(1024) not null,
  class int references $.Classes(id) on delete cascade,
  static bool not null,
  returnType int not null references $.Types(id) on delete cascade,
  foreign key (id, name) references $.Values(id, name) on delete cascade
);

create table $.Arguments (
  func int references $.Methods(id) on delete cascade,
  index int not null,
  type int not null references $.Types(id) on delete cascade,
  primary key (func, index)
);

-- Relations.

create table $.Inherits (
  parent int references $.Classes(id) on delete cascade,
  child int references $.Classes(id) on delete cascade,
  primary key (parent, child)
);

create table $.Calls (
  caller int references $.Methods(id) on delete cascade,
  callee int references $.Methods(id) on delete cascade,
  primary key (caller, callee)
);

create table $.Reads (
  func int references $.Methods(id) on delete cascade,
  var int references $.Fields(id) on delete cascade,
  primary key (func, var)
);

create table $.Writes (
  func int references $.Methods(id) on delete cascade,
  var int references $.Fields(id) on delete cascade,
  primary key (func, var)
);
