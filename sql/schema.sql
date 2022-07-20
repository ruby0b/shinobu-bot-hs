BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "voice_to_text" (
	"id"	INTEGER NOT NULL PRIMARY KEY,
	"voice_id"	INTEGER NOT NULL,
	"text_id"	INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS "banned_patterns" (
	"id" INTEGER NOT NULL PRIMARY KEY,
	"regex"	TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS "regex_reactions" (
	"id" INTEGER NOT NULL PRIMARY KEY,
	"regex"	TEXT NOT NULL,
	"reaction" TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS "consumed_media" (
	"user"	INTEGER NOT NULL,
	"type"	TEXT NOT NULL,
	"id"	INTEGER NOT NULL,
	"amount"	INTEGER NOT NULL,
	PRIMARY KEY("user","type","id"),
	FOREIGN KEY("user") REFERENCES "user"("id")
);
CREATE TABLE IF NOT EXISTS "waifu" (
	"id"	INTEGER NOT NULL,
	"character"	INTEGER NOT NULL,
	"rarity"	INTEGER NOT NULL,
	"user"	INTEGER NOT NULL,
	FOREIGN KEY("rarity") REFERENCES "rarity"("value") ON UPDATE CASCADE,
	FOREIGN KEY("character") REFERENCES "character"("id"),
	FOREIGN KEY("user") REFERENCES "user"("id"),
	PRIMARY KEY("id")
);
CREATE TABLE IF NOT EXISTS "rarity" (
	"value"	INTEGER NOT NULL,
	"name"	TEXT NOT NULL UNIQUE,
	"colour"	INTEGER NOT NULL,
	"weight"	REAL NOT NULL,
	"refund"	INTEGER NOT NULL CHECK("refund" >= 0),
	"upgrade_cost"	INTEGER,
	"auto_upgrade"	INTEGER NOT NULL DEFAULT 0 CHECK("auto_upgrade" IN (0, 1)),
	PRIMARY KEY("value")
);
CREATE TABLE IF NOT EXISTS "batch" (
	"name"	TEXT NOT NULL,
	PRIMARY KEY("name")
);
CREATE TABLE IF NOT EXISTS "batch_in_pack" (
	"batch"	TEXT NOT NULL,
	"pack"	TEXT NOT NULL,
	"weight"	REAL NOT NULL,
	FOREIGN KEY("pack") REFERENCES "pack"("name") ON UPDATE CASCADE ON DELETE CASCADE,
	FOREIGN KEY("batch") REFERENCES "batch"("name") ON UPDATE CASCADE ON DELETE CASCADE,
	PRIMARY KEY("batch","pack")
);
CREATE TABLE IF NOT EXISTS "pack" (
	"name"	TEXT NOT NULL,
	"cost"	INTEGER NOT NULL DEFAULT 100,
	"description"	TEXT NOT NULL,
	"start_date"	TEXT NOT NULL DEFAULT CURRENT_DATE,
	"end_date"	TEXT,
	PRIMARY KEY("name")
);
CREATE TABLE IF NOT EXISTS "character" (
	"id"	INTEGER NOT NULL,
	"name"	TEXT NOT NULL,
	"image_url"	TEXT,
	"series"	TEXT NOT NULL,
	"rarity"	INTEGER NOT NULL DEFAULT 1,
	"batch"	TEXT,
	FOREIGN KEY("rarity") REFERENCES "rarity"("value") ON UPDATE CASCADE,
	PRIMARY KEY("id" AUTOINCREMENT),
	FOREIGN KEY("batch") REFERENCES "batch"("name") ON UPDATE CASCADE
);
CREATE TABLE IF NOT EXISTS "user" (
	"id"	INTEGER NOT NULL,
	"balance"	INTEGER NOT NULL DEFAULT 10 CHECK("balance" >= 0),
	"last_withdrawal"	TEXT NOT NULL DEFAULT CURRENT_DATE,
	"birthday"	TEXT,
	"mal_username"	TEXT UNIQUE,
	"is_owner"	BOOLEAN NOT NULL DEFAULT 0 CHECK("is_owner" IN (0, 1)),
	PRIMARY KEY("id")
);
CREATE UNIQUE INDEX IF NOT EXISTS "idx_waifu" ON "waifu" (
	"character"	DESC,
	"user"	ASC
);
COMMIT;
