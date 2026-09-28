identification division.
program-id. useec85.
*> USE AFTER EXCEPTION CONDITION is COBOL 2002: refused under -std=85.
procedure division.
declaratives.
d1 section.
    use after exception condition ec-size.
end declaratives.
m section.
p.
    stop run.
