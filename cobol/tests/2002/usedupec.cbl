identification division.
program-id. usedupec.
*> The same exception-name in two USE statements is allowed: the
*> declaratives are examined in the order written and the first that
*> qualifies runs (2023 14.9.49.4 rule 3; no syntax rule forbids it --
*> before the sweep this compiler refused it).
*> No oracle: GnuCOBOL 4 does not implement exception declaratives.
procedure division.
declaratives.
d1 section.
    use after exception condition ec-user-a.
d1p.
    display "  first USE for EC-USER-A".
d2 section.
    use after exception condition ec-user-a.
d2p.
    display "  second USE (not expected)".
end declaratives.
m section.
m1.
>>TURN EC-USER-A CHECKING ON
    raise exception ec-user-a
    display "after"
    stop run.
