* An error no handler takes, in a program: dBASE III's Cancel, Ignore, or
* Suspend (test_error_ask answers each in turn)
x = 1
? "before"
DO ERRASK2
? "back in ERRASK", x
