/* s32-cobc -- COBOL 85 for SLOW-32.  Host cross-compiler.
 *
 * Reads ANSI X3.23-1985 COBOL (fixed or free reference format) plus the
 * implementor modules listed in docs/dialect.md, and emits SLOW-32
 * assembler for slow32asm / s32-ld.  Not SSA, not BURG: the IR is the
 * symbol table (Sym[]), and each verb is a lowering against it -- an inline
 * sequence for the hot cases, otherwise a call into libcob with a
 * descriptor the compiler built (libcob/cobrt.h).  docs/architecture.md.
 *
 * Stage 2 (docs/plan.md): the Data Division as a tree -- groups,
 * REDEFINES, OCCURS with subscripts, 77, 88, qualification -- the
 * conversion matrix behind MOVE, the arithmetic statements on a scaled-i64
 * numeric stack with COMP-integer hot cases inline, conditions, IF,
 * every PERFORM form, GO TO, SET.  Stage 3: edited MOVE and de-edit
 * through the runtime's software editor (libcob/kern.h), COMPUTE with
 * arithmetic expressions (also as condition operands), ROUNDED, ON SIZE
 * ERROR, REMAINDER.  Stage 4: SELECT/FD, line sequential and fixed
 * sequential files (OPEN, CLOSE, READ, WRITE), STRING, the case
 * intrinsics.  Stage 5: INDEXED files -- READ KEY / NEXT, WRITE, REWRITE,
 * DELETE, START, INVALID KEY.  Stage 6: several program units per
 * source, LINKAGE SECTION, PROCEDURE DIVISION USING, CALL on the SLOW-32
 * C ABI (BY REFERENCE / BY VALUE / RETURNING at the C seam), so COBOL, C
 * and Fortran link with no glue.  Stage 7: Report Writer, the cheap
 * half -- RD with PAGE LIMIT / HEADING / FIRST and LAST DETAIL, PAGE
 * HEADING and DETAIL groups, LINE / COLUMN / SOURCE / VALUE, INITIATE /
 * GENERATE / TERMINATE, rendered per GENERATE site against a page engine
 * in libcob (docs/report-writer.md).  Stage 8: SCREEN SECTION -- a table
 * of slots per 01, DISPLAY paints and ACCEPT runs the focus loop, on the
 * term service (docs/screen.md).  Stage 9, what menu and taskdt drag
 * in: EVALUATE, INSPECT, INITIALIZE, reference modification with
 * arithmetic, FUNCTION LENGTH and CURRENT-DATE.  Stage 10: sequential
 * mode V -- RECORDING MODE V, RECORD CONTAINS m TO n, RECORD IS VARYING
 * DEPENDING ON, or unequal 01s -- with the IBM RDW on disk.  Stage 12:
 * COPY (the Library module) as token-stream inclusion, copybooks found
 * through -I.  Unimplemented is a diagnostic, never silence.
 */
/* localtime()'s tm_gmtoff (FUNCTION WHEN-COMPILED's zone offset) is a BSD
 * extension that POSIX only standardised in 2024.  Apple's headers expose it
 * under -std=c99; glibc's hide it unless a feature-test macro asks, so
 * without this the compiler does not build on Linux at all. */
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <unistd.h>
#include <ctype.h>
#include <limits.h>
#include <strings.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include "picture.h"
#include "lex.h"            /* the token scanner (lex.rl, Ragel -G2; lex_scan.c) */
#include "../libcob/wide.h"    /* 31 digits: the runtime's 128-bit arithmetic, for VALUE and literals */
#include "../libcob/ieee.h"    /* the standard floating-point formats (2014): a VALUE clause encoded at compile time */
#include "../libcob/dtfmt.h"   /* the 2014 date and time formats: a format literal checked, the result sized */
#include "../libcob/cobrt.h"
#define SCREDIT_EXPAND_ONLY
#include "../libcob/scredit.h"  /* se_expand_picture: a screen field's picture, a symbol a column */
#include "../../common/s32utf.h"   /* the one Unicode model: coding, width, clusters (cobol ISSUES-94) */

#define VERSION "0.63 (stage 63: IF module)"

#include "cobc/diag.h"
#include "cobc/reader.h"
#include "cobc/tokenizer.h"
#include "cobc/copy.h"
#include "cobc/typedef.h"
#include "cobc/cursor.h"
#include "cobc/numlit.h"
#include "cobc/symtab.h"
#include "cobc/data.h"
#include "cobc/data_rules.h"
#include "cobc/layout.h"
#include "cobc/emit.h"
/* stage08's HIR backend, a copy (src/hir; the ruling and the contract are
 * in cobc/hir_contract.h).  The lowering that feeds it is cobc/lower.h,
 * after the statement compilers it reads. */
#include "cobc/hir_contract.h"
#pragma GCC diagnostic push            /* the copy is kept byte-comparable with its original, not warning-clean here */
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#pragma GCC diagnostic ignored "-Wpointer-to-int-cast"
#if defined(__GNUC__) && !defined(__clang__)       /* gcc's own: clang warns about the pragma instead */
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif
#include "hir/hir.h"
#include "hir/hir_ssa.h"
#include "hir/hir_opt.h"
#include "hir/hir_licm.h"
#include "hir/hir_burg.h"
#include "hir/hir_regalloc.h"
#include "hir/hir_codegen.h"
#pragma GCC diagnostic pop
#include "cobc/operand.h"
#include "cobc/const.h"
#include "cobc/operand_parse.h"
#include "cobc/args.h"
#include "cobc/udf.h"
#include "cobc/cond.h"
#include "cobc/stmt.h"
#include "cobc/display.h"
#include "cobc/move.h"
#include "cobc/arith.h"
#include "cobc/arith_reg.h"
#include "cobc/expr.h"
#include "cobc/control.h"
#include "cobc/pcensus.h"
#include "cobc/loopreg.h"
#include "cobc/sort.h"
#include "cobc/goto_set.h"
#include "cobc/fileio.h"
#include "cobc/string_stmt.h"
#include "cobc/call.h"
#include "cobc/report.h"
#include "cobc/helpers.h"
#include "cobc/verbs.h"
#include "cobc/dispatch.h"
#include "cobc/lower.h"
#include "cobc/census.h"
#include "cobc/native.h"
#include "cobc/esql.h"
#include "cobc/divisions.h"
#include "cobc/driver.h"
