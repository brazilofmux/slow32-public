* Commands: SAVE TO <file> ALL LIKE <skel> and SAVE ALL LIKE <skel> TO
* <file> (RL's order), RESTORE FROM <file> [ADDITIVE]; the four saved types.
* Names stay within ten characters: Clipper keeps ten significant, Harbour
* keeps them all, and a .MEM holds ten
PRIVATE frm_cols := 0, frm_wid := 80, frm_ttl := "Sales by month", frm_dbl := .F.
PRIVATE frm_dt := CTOD("03/15/1992"), kept := "kept"
SAVE ALL LIKE frm_* TO csr_a
SAVE TO csr_b ALL LIKE frm_c*
frm_cols := 24
frm_wid := 1
frm_ttl := "changed"
RESTORE FROM csr_a ADDITIVE
? frm_cols, frm_wid, frm_ttl, frm_dbl, frm_dt, kept
frm_cols := 24
RESTORE FROM csr_b ADDITIVE
? frm_cols, frm_wid, kept
? FILE("csr_a.mem"), FILE("csr_b.mem")
FERASE("csr_a.mem")
FERASE("csr_b.mem")
