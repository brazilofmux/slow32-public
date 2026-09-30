identification division.
program-id. copyquote.
*> Matching literals in COPY ... REPLACING and REPLACE (2002 and 2023
*> 7.2.3.4 rule 9c4, 7.2.4.4 rule 8c4): the two quotation marks match
*> each other, and a doubled quote inside is one; so =="abc"== replaces
*> 'abc'.  This compiler takes the apostrophe in COBOL 85 too, and the
*> rule with it.  A hexadecimal literal is not the characters it spells:
*> X"616263" stays.  GnuCOBOL matches a literal only in the quote it was
*> written with (.oracle-expected; docs/oracles.md).
data division.
working-storage section.
01 a pic x(3) value "abc".
replace =="abc"== by =="rep"== =="it""s"== by =="its"==.
01 b pic x(3) value 'abc'.
01 c pic x(3) value X"616263".
01 d pic x(4) value 'it''s'.
replace off.
procedure division.
    display a " " b " " c " " d
    stop run.
