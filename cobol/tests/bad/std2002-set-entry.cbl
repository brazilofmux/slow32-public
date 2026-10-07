identification division.
program-id. p26.
*> SET ... TO ENTRY is IBM's and Micro Focus's; the standard gives a
*> program-pointer its value by ADDRESS OF PROGRAM (2023 8.4.3.13,
*> 14.9.39 format 9).
data division.
working-storage section.
01 p usage program-pointer.
procedure division.
    set p to entry 'abc'
    goback.
