* Commands: SET options a program sets and expects no complaint about --
* DECIMAL (the abbreviation), DEFAULT, DELIMITERS, EXCLUSIVE, FORMAT
* (CA's DOT.PRG resets every one of them at its prompt)
SET DECIMAL TO 3
? 1 / 4
SET DECIMALS TO 2
SET DEFAULT TO
SET DELIMITERS OFF
SET DELIMITERS TO "[]"
SET DELIMITERS TO DEFAULT
SET DELIMITERS ON
SET EXCLUSIVE ON
SET EXCLUSIVE OFF
SET FORMAT TO
? "still here", 1 / 4
