REM === PRINT USING Tests ===

REM Basic numeric format
PRINT USING "###.##"; 3.14159
PRINT USING "###.##"; 42.5
PRINT USING "###.##"; -7.3

REM Dollar sign
PRINT USING "$$###.##"; 99.5
PRINT USING "$$###.##"; 1234.56

REM Asterisk fill
PRINT USING "**###.##"; 42.5

REM String formats
PRINT USING "!"; "Hello"
PRINT USING "&"; "Hello World"
PRINT USING "\     \"; "Hello"
PRINT USING "\     \"; "Hi"

REM Mixed literal text and format
PRINT USING "Value: ###.## units"; 42.5

REM Integer value in numeric format
PRINT USING "###"; 42
PRINT USING "###"; 7

REM Large numbers (exceed unsigned 32-bit range)
PRINT USING "##############.##"; 5000000000.0
PRINT USING "##############.##"; -5000000000.0

REM Scientific notation
PRINT USING "##.##^^^^"; 12345.6
PRINT USING "##.##^^^^"; 0.00789
