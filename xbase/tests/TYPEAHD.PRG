* A key typed while the program runs is kept for the next read, as
* dBASE's type-ahead keeps it: SET ESCAPE's check between statements
* took it and threw it away
x = 0
DO WHILE NEXTKEY() = 0
   x = 0
   x = 1
   x = 2
   x = 3
   x = 4
   x = 5
   x = 6
   x = 7
   x = 8
   x = 9
ENDDO
? INKEY()
QUIT
