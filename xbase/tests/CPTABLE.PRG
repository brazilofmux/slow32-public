* Tables in UTF-8 (docs/plans/xbase-utf8.md, step 4): a table marked CP437
* or Windows 1252 (header byte 29) is read into the program's codepage and
* written back into its own -- a character it lacks as the nearest it has
* (ruling 2) -- and an unmarked table is the program's, its fields cut on a
* character, never through one.  CPDBF(), CPCURRENT() as FoxPro's.
SET TALK OFF
SET SAFETY OFF
SET CODEPAGE TO "UTF8"
COPY FILE codepage/CPT437.DBF TO CPW437.DBF
COPY FILE codepage/CPT1252.DBF TO CPW1252.DBF
COPY FILE codepage/CPTNONE.DBF TO CPWNONE.DBF
USE CPW437
? "437", CPDBF(), CPCURRENT(), "[" + TRIM(NAME) + "]", LEN(TRIM(NAME))
GO 2
REPLACE NAME WITH "“Gödel”"
? "437 back", "[" + TRIM(NAME) + "]"
USE CPW1252
? "1252", CPDBF(), "[" + TRIM(NAME) + "]"
GO 2
REPLACE NAME WITH "Ærø €5"
? "1252 back", "[" + TRIM(NAME) + "]"
USE CPWNONE
? "none", CPDBF(), "[" + TRIM(NAME) + "]"
GO 2
REPLACE NAME WITH "日本語"
? "none cut", "[" + TRIM(NAME) + "]", LEN(TRIM(NAME))
USE
* index keys are the table's bytes: CP437's for CPW437, found by UTF-8 SEEKs
USE CPW437
INDEX ON NAME TO CPW437
SEEK "Müller"
? "437 seek", FOUND(), RECNO()
SEEK "Mü"
? "437 prefix", FOUND(), RECNO()
USE CPWNONE
INDEX ON NAME TO CPWNONE
SEEK "日本"
? "none seek", FOUND(), RECNO()
USE
SET CODEPAGE TO "CP437"
USE CPW437
GO 2
? "437 bytes", BASC(BSUBSTR(NAME, 1)), BASC(BSUBSTR(NAME, 3)), BASC(BSUBSTR(NAME, 7))
USE CPW1252
GO 2
? "1252 bytes", BASC(NAME), BASC(BSUBSTR(NAME, 3)), BASC(BSUBSTR(NAME, 5))
USE
