IDENTIFICATION DIVISION.
PROGRAM-ID. EcLoc.
*> EXCEPTION-LOCATION and EXCEPTION-FILE, and their national forms
*> (COBOL 2002 15.23-15.26; cobol ISSUES-65), whose results are as long
*> as their contents.  The location is "program; paragraph OF section;
*> line" with the names as written in the source, only when checking was
*> turned on WITH LOCATION -- one space otherwise, and one space before
*> any condition.  The line is the statement's number, or copybook:number
*> for a statement copied in.  EXCEPTION-FILE is the I-O status and the
*> file-name as written in SELECT for an EC-I-O condition, two zeros
*> otherwise.  No oracle (ecraise).
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT Ledger-File ASSIGN TO "tmp/ecloc.dat" ORGANIZATION SEQUENTIAL.
DATA DIVISION.
FILE SECTION.
FD  Ledger-File.
01  Ledger-Rec   PIC X(4).
PROCEDURE DIVISION.
DECLARATIVES.
Any-Condition SECTION.
    USE AFTER EXCEPTION CONDITION EC-ALL.
Show-It.
    DISPLAY "  " FUNCTION EXCEPTION-STATUS(1:18)
            " [" FUNCTION EXCEPTION-LOCATION "]"
            " [" FUNCTION EXCEPTION-FILE "]".
END DECLARATIVES.
Main-Line SECTION.
Start-Up.
    DISPLAY "before any: [" FUNCTION EXCEPTION-LOCATION "] ["
            FUNCTION EXCEPTION-FILE "] " FUNCTION LENGTH(FUNCTION EXCEPTION-LOCATION)
>>TURN EC-USER-PLAIN CHECKING ON
    RAISE EXCEPTION EC-USER-PLAIN
>>TURN EC-USER CHECKING ON WITH LOCATION
    RAISE EXCEPTION EC-USER-HERE.
Second-Para.
    COPY ecloc-raise.
    DISPLAY "national: [" FUNCTION EXCEPTION-LOCATION-N "] "
            FUNCTION LENGTH(FUNCTION EXCEPTION-LOCATION-N) " "
            FUNCTION BYTE-LENGTH(FUNCTION EXCEPTION-LOCATION-N).
Lone-Section SECTION.
>>TURN EC-I-O CHECKING ON WITH LOCATION
    OPEN OUTPUT Ledger-File  WRITE Ledger-Rec FROM "abcd"  CLOSE Ledger-File
    OPEN INPUT Ledger-File
    READ Ledger-File
    READ Ledger-File
    DISPLAY "file: [" FUNCTION EXCEPTION-FILE "] ["
            FUNCTION EXCEPTION-FILE-N "] " FUNCTION LENGTH(FUNCTION EXCEPTION-FILE-N)
    CLOSE Ledger-File
    STOP RUN.
