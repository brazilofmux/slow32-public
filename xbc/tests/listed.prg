* The files a program reaches named on the command line too, as a
* Clipper make file lists them: each is compiled once, the library's
* routines once and the DO'd file as the procedure of its name.
SET PROCEDURE TO LISTLIB
? Twice(21)
DO listsub WITH "from main"
? "main again"
