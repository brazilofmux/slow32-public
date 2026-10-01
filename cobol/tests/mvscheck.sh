#!/bin/bash
# mvscheck.sh PROG.cbl... -- compile and run fixed-format programs under
# IBM's ANS COBOL (IKFCBL00, the MVT compiler on MVS 3.8j, 1968 language
# with IBM extensions), a third witness beside GnuCOBOL and MS COBOL 5.0
# (docs/oracles.md).  It is pre-74: no INSPECT, no '/' in a PICTURE.
# tests/witness holds programs written for it.
#
# The system is the live TK5 rig, operated by ~/mvsops: this script never
# boots or shuts it down.  Bring it up with tk5-up, run this, take it down
# with tk5-down (~/mvsops/CLAUDE.md).  Each program is one job (COBUCL
# into a temporary library, then a RUN step) through tk5-run, as
# ~/cobc370/bench/run.sh does; nothing permanent is written.  The run
# output is printed beside NAME.expected (ours) when that exists.
# Signed DISPLAY output is zoned: the last digit carries the sign
# (J-R are -1 to -9, } is -0, A-I and { are positive).
#
#   tk5-up; cobol/tests/mvscheck.sh cobol/tests/witness/divremu.cbl; tk5-down
set -eu
MVSOPS=${MVSOPS:-$HOME/mvsops}
export PATH="$MVSOPS/bin:$PATH"
command -v tk5-run >/dev/null || { echo "mvscheck: no tk5-run under $MVSOPS/bin" >&2; exit 2; }
tk5-status 2>/dev/null | sed -n '/^== Hercules/,/^$/p' | grep -q "not running" &&
    { echo "mvscheck: MVS is down -- tk5-up first (and tk5-down after)" >&2; exit 2; }
M="$(mktemp -d "${TMPDIR:-/tmp}/mvscheck.XXXXXX")"
trap 'case "$M" in */mvscheck.*) rm -rf -- "$M";; esac' EXIT
extract() {   # extract JOB-OUTPUT EXPECTED
    local re
    re=$(cut -c1-2 "$2" 2>/dev/null | sort -u | sed 's/[^A-Za-z0-9 ]/\\&/g' | paste -sd'|' -)
    [ -n "$re" ] || re='[0-9A-Z] '
    grep -a -E "^($re)" "$1" || true
}
for src in "$@"; do
    [ -f "$src" ] || { echo "== $src: missing"; continue; }
    name=$(basename "$src" .cbl)
    mem=$(echo "$name" | cut -c1-7 | tr 'a-z' 'A-Z')
    pid=$(sed -n 's/^ \{7\}PROGRAM-ID\. *\([A-Z0-9-]*\)\..*/\1/p' "$src" | head -1)
    { printf '//%-8s JOB (5161A020,1A11),%s,\n//             CLASS=A,MSGCLASS=Z,MSGLEVEL=(1,1),\n//             USER=HERC01,PASSWORD=@HERC01PW@\n' "A$mem" "'S32WIT'"
      printf '//S1      EXEC COBUCL\n//COB.SYSIN DD *\n'; cut -c1-72 "$src"; printf '/*\n'
      printf '//LKED.SYSLMOD DD DSN=&&BL(%s),DISP=(NEW,PASS),UNIT=SYSDA,\n//             SPACE=(CYL,(1,1,5)),DCB=(RECFM=U,BLKSIZE=19069)\n' "$pid"
      printf '//RUN     EXEC PGM=%s\n//STEPLIB DD DSN=&&BL,DISP=(OLD,PASS)\n//SYSOUT  DD SYSOUT=Z\n' "$pid"; } > "$M/$name.jcl"
    tk5-run "$M/$name.jcl" "$M/$name.out" >/dev/null 2>&1 || true
    echo "== $name: ANS COBOL (MVT)"
    grep -a "IKF[0-9]" "$M/$name.out" | grep -v "A-MARGIN" | sed 's/^/   diag /' || true
    grep -a "IEF453I\|NOT RUN" "$M/$name.out" | sed 's/^/   /' || true
    # the program's DISPLAY lines: those starting as the expected lines
    # start (a label and a space), anchored, so the listing never matches
    exp="${src%.cbl}.expected"
    out=$(extract "$M/$name.out" "$exp")
    if [ -f "$exp" ]; then paste -d'|' <(printf '%s\n' "$out") "$exp" | sed 's/^/   /'; else printf '%s\n' "$out" | sed 's/^/   /'; fi
done
