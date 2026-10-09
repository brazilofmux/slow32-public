*> an ORDER TABLE naming a table other than the one here (2023 12.3.7 rule 17)
identification division.
program-id. ordunknown.
environment division.
configuration section.
special-names.
    order table ebcdic-order is "IBM_037_TABLE".
procedure division.
    stop run.
