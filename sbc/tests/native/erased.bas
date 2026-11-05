' What ERASE and CLEAR leave (QBasic 1.1 printed each of these lines): a
' static array stays dimensioned and is zeroed; a dynamic one is gone, any
' use out of range, until something makes it again
DECLARE SUB inproc ()
DECLARE SUB keeper ()
DECLARE SUB sharedgo ()
TYPE pair
num AS INTEGER
txt AS STRING * 2
END TYPE
CONST top = 4
ON ERROR GOTO caught
DIM big(20), bystr$(3), byfix(3) AS STRING * 2, byrec(3) AS pair, bycon(top)
REDIM SHARED dyn(3)
big(15) = 5: bystr$(1) = "abc": byfix(1) = "ab": byrec(1).num = 4: byrec(1).txt = "xy"
bycon(top) = 7
ERASE big, bystr$, byfix, byrec, bycon
PRINT big(15); UBOUND(big); "["; bystr$(1); "]"; ASC(byfix(1)); LEN(byfix(1))
PRINT byrec(1).num; ASC(byrec(1).txt); LEN(byrec(1).txt); bycon(top); UBOUND(bycon); seen
sharedgo
x = dyn(1): PRINT "read"; seen: seen = 0
dyn(1) = 2: PRINT "write"; seen: seen = 0
x = UBOUND(dyn): PRINT "bound"; seen: seen = 0
ERASE dyn: PRINT "again"; seen
REDIM dyn(5): dyn(5) = 8: PRINT dyn(5); UBOUND(dyn)
nn = 2
DIM runtime(nn, nn)
ERASE runtime
x = runtime(1, 1): PRINT "dim of a variable"; seen: seen = 0
DIM runtime(nn + 1, nn): runtime(3, 1) = 9: PRINT runtime(3, 1); UBOUND(runtime)
implied(3) = 1
ERASE implied
PRINT implied(3); UBOUND(implied); seen
CALL inproc: PRINT "local"; seen: seen = 0
CALL keeper: PRINT "its static"; seen: seen = 0
big(15) = 6: byfix(2) = "cd": byrec(2).num = 3: implied(2) = 2
REDIM dyn(3): dyn(1) = 4
CLEAR
ON ERROR GOTO caught
PRINT big(15); UBOUND(big); ASC(byfix(2)); LEN(byfix(2)); byrec(2).num; implied(2); UBOUND(implied)
x = dyn(1): PRINT "cleared"; seen: seen = 0
REDIM dyn(2): PRINT UBOUND(dyn)
END
caught:
seen = ERR
RESUME NEXT
SUB inproc
DIM mine(3)
mine(1) = 2
ERASE mine
PRINT mine(1)
END SUB
SUB keeper
STATIC kept()
DIM kept(3)
kept(1) = 2
ERASE kept
PRINT kept(1); UBOUND(kept)
END SUB
SUB sharedgo
ERASE dyn
END SUB
