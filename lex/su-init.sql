--- Copyright (c) 2003 - 2006
--- Benjamin Waldron, Fabre Lambeau, Stephan Oepen;
--- see `licence.txt' for conditions.
--- FCB fields taken from japanese/lex/lexdb.fld
SET client_min_messages TO warning;

-- DFN -- don't change this!
CREATE TABLE dfn (
		slot TEXT,
		field TEXT,
		path TEXT,
		type TEXT,
	PRIMARY KEY (slot,field));

-- LEX -- customize this...
CREATE TABLE lex (
-- BUILT-IN FIELD (do not change!!!)
name TEXT NOT NULL,
-- USER-DEFINED FIELDS
userid TEXT DEFAULT user NOT NULL,
modstamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
type TEXT,
orthography TEXT,
keyrel TEXT,
keytag TEXT,
altkey TEXT,
alt2key TEXT,
altkeytag TEXT,
compkey TEXT,
idiom TEXT,
pronunciation TEXT,
comments TEXT,
lang TEXT,
country TEXT,
dialect TEXT,
confidence real DEFAULT 1,
source TEXT, 
	PRIMARY KEY (name));