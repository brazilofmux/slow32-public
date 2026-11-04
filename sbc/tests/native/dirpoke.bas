' directories, POKE and PEEK, DEF SEG, LPRINT in s32-bc
MKDIR "bcdir.tmp": CHDIR "bcdir.tmp": CHDIR "..": RMDIR "bcdir.tmp"
DEF SEG = 16: POKE 3, 300: PRINT PEEK(3)
DEF SEG: PRINT PEEK(259); PEEK(3)
LPRINT "to the printer"; 5
PRINT "done"
