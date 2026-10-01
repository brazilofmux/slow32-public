identification division.
program-id. ifc.
*> A function-identifier is not a receiving operand (2023 8.4.3.2.3 rule 1).
data division.
working-storage section.
01 s pic x(4) value "abcd".
procedure division.
    inspect function trim(s) converting "a" to "b"
    stop run.
