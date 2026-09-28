identification division.
program-id. rec85 is recursive.
*> RECURSIVE is COBOL 2002: under -std=85, the default, it is refused
*> with a message naming the switch (docs/standards.md, Stage B).
procedure division.
    stop run.
