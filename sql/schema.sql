BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "voice_to_text" (
	"id"	INTEGER NOT NULL PRIMARY KEY,
	"voice_id"	INTEGER NOT NULL,
	"text_id"	INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS "regex_reactions" (
	"id" INTEGER NOT NULL PRIMARY KEY,
	"regex"	TEXT NOT NULL,
	"reaction" TEXT NOT NULL
);
COMMIT;
