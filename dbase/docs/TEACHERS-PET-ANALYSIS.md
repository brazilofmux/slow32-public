# Teacher's Pet — Gap Analysis

Cross-reference of every dBase III feature used in the akron/ "Teacher's Pet"
programs (1986) against the current clone implementation.

## Overview

Teacher's Pet is a gradebook management system for school teachers.
18 PRG files, 9 DBF files, multi-index, multi-work-area, full-screen UI,
printer output, and macro-driven dynamic variable naming.

## Feature-by-Feature Status

### SET Commands Used

| SET Command | Used In | Status | Notes |
|-------------|---------|--------|-------|
| SET TALK OFF | tp.prg, most files | HAVE | |
| SET ECHO OFF | sane.prg | MISSING | No-op ok (we don't echo by default) |
| SET BELL OFF | sane.prg | HAVE | |
| SET MENU OFF | sane.prg | MISSING | No-op ok (no menu bar) |
| SET SAFETY OFF | sane.prg | HAVE | |
| SET STATUS OFF | sane.prg | MISSING | No-op ok (no status bar) |
| SET DELETED ON | sane.prg | HAVE | |
| SET SCOREBOARD OFF | sane.prg | MISSING | No-op ok (no scoreboard) |
| SET EXACT OFF | sane.prg | HAVE | |
| SET COLOR TO W | tp.prg, many | HAVE | Need W+ (high intensity) |
| SET COLOR TO W+ | tpeg.prg, tppg.prg | PARTIAL | Need +/\* modifiers parsed |
| SET DEVICE TO SCREEN | tppg.prg, tppp.prg | MISSING | **Critical for reports** |
| SET DEVICE TO PRINTER | tppg.prg, tppp.prg | MISSING | **Critical for reports** |
| SET CONSOLE OFF | tppg.prg, tppp.prg | HAVE | |
| SET CONSOLE ON | tppg.prg | HAVE | |
| SET FUNCTION n TO str | tp.prg | MISSING | No-op ok (no function keys) |

**Gap count: 6 missing, 4 are harmless no-ops, 1 is partial, 1 is critical**

### File & Database Commands

| Command | Used In | Status | Notes |
|---------|---------|--------|-------|
| USE file INDEX i1,i2 ALIAS name | tplib.prg | HAVE | |
| USE (close) | various | HAVE | |
| CLOSE DATABASES | tp.prg | MISSING | Need as alias for close-all |
| SELECT n | various | HAVE | |
| SELECT alias_name | tpeg.prg, tppg.prg | HAVE | |
| GO n / GO TOP / GO BOTTOM | various | HAVE | |
| SKIP [n] | various | HAVE | |
| SEEK value | various | HAVE | |
| LOCATE FOR condition | tpeg.prg, tppg.prg | HAVE | |
| CONTINUE | tpeg.prg, tppg.prg | HAVE | |
| EOF() / BOF() | various | HAVE | |
| APPEND BLANK | various | HAVE | |
| DELETE | various | HAVE | |
| DELETE FOR condition | tpeg.prg | HAVE | |
| PACK | tppack.prg | HAVE | |
| REPLACE field WITH value | various | HAVE | |
| REPLACE f1 WITH v1, f2 WITH v2 | tpes.prg, tpeg.prg | HAVE | Multiple REPLACE |
| INDEX ON expr TO file | tpndx.prg | HAVE | |
| SET ORDER TO n | tpeg.prg | HAVE | |
| SET FILTER TO condition | tpeg.prg, tpqd.prg | HAVE | |
| COPY TO file FOR cond | tppg.prg | HAVE | |
| COPY FILE src TO dst | tpbp.prg | HAVE | |
| SORT TO file ON field | tppg.prg | HAVE | |
| ERASE filename | tppg.prg | HAVE | |
| ZAP | tpsy.prg | HAVE | |

**Gap count: 1 missing (CLOSE DATABASES)**

### Control Flow

| Feature | Used In | Status | Notes |
|---------|---------|--------|-------|
| DO WHILE...ENDDO | everywhere | HAVE | |
| IF...ELSE...ENDIF | everywhere | HAVE | |
| DO CASE...ENDCASE | tp.prg, tppg.prg | HAVE | |
| EXIT | tpeg.prg, tppg.prg | HAVE | |
| LOOP | tpeg.prg | HAVE | |
| DO procedure | everywhere | HAVE | |
| DO proc WITH a,b | tpeg.prg, tppg.prg | HAVE | |
| PROCEDURE name | tplib.prg | HAVE | |
| PARAMETERS p1,p2 | tplib.prg | HAVE | |
| PRIVATE var1,var2 | everywhere | HAVE | |
| RETURN | tplib.prg | HAVE | |

**Gap count: 0**

### Memory Variables

| Feature | Used In | Status | Notes |
|---------|---------|--------|-------|
| var = expr | everywhere | HAVE | |
| STORE expr TO var | various | HAVE | |
| PRIVATE var1,var2 | everywhere | HAVE | |
| PUBLIC var | tpqd.prg | HAVE | |
| RELEASE ALL LIKE T_* | tp.prg | MISSING | **Need wildcard pattern** |
| PUBLIC &macro_var | tpqd.prg | PARTIAL | Need PUBLIC with macro expansion |

**Gap count: 1 missing (RELEASE ALL LIKE), 1 partial**

### Screen I/O

| Feature | Used In | Status | Notes |
|---------|---------|--------|-------|
| CLEAR | everywhere | HAVE | |
| @ row,col SAY "text" | everywhere | HAVE | |
| @ row,col SAY field | everywhere | HAVE | |
| @ row,col SAY expr PICTURE mask | tppg.prg | HAVE | |
| @ row,col GET var | tpes.prg, tpeg.prg | HAVE | |
| @ row,col GET var PICTURE mask | tpes.prg, tpeg.prg | HAVE | |
| @ row,col GET var RANGE lo,hi | tpqd.prg, tpso.prg | MISSING | **Need RANGE clause** |
| READ | various | HAVE | |

**Gap count: 1 missing (RANGE on GET)**

### Functions Used

| Function | Used In | Status | Notes |
|----------|---------|--------|-------|
| EOF() | everywhere | HAVE | |
| BOF() | tpeg.prg | HAVE | |
| RECNO() | tpeg.prg | HAVE | |
| RECCOUNT() | tpes.prg | HAVE | |
| DELETED() | tpes.prg | HAVE | |
| FOUND() | tppg.prg | HAVE | |
| SUBSTR() | tppg.prg, tpeg.prg | HAVE | |
| TRIM() | tpeg.prg, tppg.prg | HAVE | |
| UPPER() | various | HAVE | |
| AT() | tppg.prg, tp.prg | HAVE | |
| LEN() | tpeg.prg | HAVE | |
| SPACE() | various | HAVE | |
| REPLICATE() | tp.prg | HAVE | |
| STR() | tpeg.prg, tppg.prg | HAVE | |
| VAL() | tpqd.prg | HAVE | |
| CHR() | tppg.prg | HAVE | |
| LEFT() | tppg.prg | HAVE | |
| RIGHT() | tppg.prg | HAVE | |
| DATE() | tppg.prg | HAVE | |
| CTOD() | tpndx.prg, tppg.prg | HAVE | |
| DTOC() | tppg.prg | HAVE | |
| DOW() | tplib.prg | HAVE | |
| CDOW() | (not used) | HAVE | |
| CMONTH() | (not used) | HAVE | |
| DAY() | (not used) | HAVE | |
| MONTH() | (not used) | HAVE | |
| YEAR() | (not used) | HAVE | |
| ROW() | tpeg.prg | MISSING | Current cursor row |
| COL() | tpeg.prg | MISSING | Current cursor column |

**Gap count: 2 missing (ROW, COL)**

### Operators Used

| Operator | Used In | Status | Notes |
|----------|---------|--------|-------|
| + (arithmetic) | everywhere | HAVE | |
| - (arithmetic) | tppg.prg | HAVE | |
| * (multiply) | tppg.prg | HAVE | |
| / (divide) | tppg.prg | HAVE | |
| + (string concat) | everywhere | HAVE | |
| = (comparison) | everywhere | HAVE | |
| <> (not equal) | various | HAVE | |
| < > <= >= | various | HAVE | |
| .AND. .OR. .NOT. | everywhere | HAVE | |
| .T. .F. | everywhere | HAVE | |
| $ (substring) | tppg.prg | HAVE | |
| -> (alias) | tpeg.prg, tppg.prg | HAVE | |
| & (macro) | tpqd.prg | HAVE | |

**Gap count: 0**

### Misc Features

| Feature | Used In | Status | Notes |
|---------|---------|--------|-------|
| * comment | various | HAVE | |
| && inline comment | various | HAVE | |
| ; line continuation | tppg.prg | HAVE | |
| &var macro substitution | tpqd.prg | HAVE | |
| &var. with period delimiter | tpqd.prg | HAVE | |
| alias->field cross-area ref | tpeg.prg, tppg.prg | HAVE | |
| Composite key SEEK | tpeg.prg, tppg.prg | HAVE | String concat as key |
| REPORT FORM file TO PRINT | tp.prg | MISSING | **Not used directly in code** |
| RUN program | (not used) | N/A | Not applicable on SLOW-32 |

## Critical Gap Summary

### Must Fix (blocks Teacher's Pet execution)

1. **SET ECHO/MENU/STATUS/SCOREBOARD/FUNCTION** — Accept as no-ops.
   These are in `sane.prg` which runs at startup. If the parser rejects
   them, the program aborts immediately.

2. **CLOSE DATABASES** — Close all open databases across all work areas.
   Used in `tp.prg` cleanup. Easy: dispatch to `cmd_close_all()`.

3. **RELEASE ALL LIKE pattern** — Release memory variables matching a
   wildcard pattern (e.g., `T_*`). Used in `tp.prg` cleanup. Need
   simple glob matching in memvar.c.

4. **SET DEVICE TO SCREEN/PRINT** — Redirects `@SAY` output between
   screen and printer. Used in `tppg.prg` (grade reports) and `tppp.prg`
   (progress reports). Without this, all report output goes nowhere.

5. **@ GET ... RANGE lo,hi** — Range validation on numeric/date input.
   Used in `tpqd.prg` (quarter definitions) and `tpso.prg` (system options).
   GETs without RANGE will still work; values just won't be validated.

6. **ROW() and COL()** — Current cursor position functions. Used in
   `tpeg.prg` for dynamic screen layout. Without these, the grade
   editing screen will misposition fields.

### Nice to Have (not blocking but improve compatibility)

7. **SET COLOR TO W+** — High-intensity color modifier. The `+` suffix
   means "bright". Currently we may not parse the `+`. Teacher's Pet
   uses `W+` (bright white) and `W` (normal white).

8. **PUBLIC with macro** — `PUBLIC T_Wk&T_wc` creates dynamically-named
   public variables. The macro should expand before PUBLIC processes
   the variable name. This may already work if macro preprocessing
   runs before command dispatch.

9. **Date arithmetic in index keys** — `INDEX ON TE_PE_NUM+STR(TE_DATE-CTOD("01/01/80"),6) TO TPTE1`.
   Needs date subtraction producing a numeric result (days between dates).
   Need to verify our expression evaluator handles date-date=number.

## Estimated Effort

| Gap | Effort | Files Changed |
|-----|--------|---------------|
| No-op SETs (5 options) | Small | set.c |
| CLOSE DATABASES | Tiny | command.c |
| RELEASE ALL LIKE | Small | memvar.c, command.c |
| SET DEVICE TO | Medium | set.c, screen.c, command.c |
| @ GET RANGE | Medium | screen.c |
| ROW()/COL() | Small | func.c, screen.c |
| SET COLOR W+ parsing | Small | screen.c or set.c |
| Date subtraction | Small | expr.c (verify) |
