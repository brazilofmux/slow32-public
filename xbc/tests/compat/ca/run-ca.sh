#!/bin/bash
# Run the CA-sample drivers: each ca-*.prg here is our own program that
# calls functions from the sources CA shipped with Clipper 5.2 (SOURCE/
# SAMPLE, SOURCE/SYS), named on its first line as "* CA: SAMPLE/X.PRG ...".
# The driver and those sources are concatenated into one program, CA's
# .CH files put beside it, and the program run under Harbour (the oracle
# image) and under s32-xbc; the outputs must match.  The CA sources are
# reference material read from $CLIPPER_SRC (default ~/refs/clipper/source)
# and never enter this tree; without them every driver is SKIP.
#
#   tests/compat/ca/run-ca.sh            # all drivers
#   tests/compat/ca/run-ca.sh ca-x.prg   # some
set -u
HERE="$(cd "$(dirname "$0")" && pwd)"
CLIP="$(cd "$HERE/../../.." && pwd)"
ROOT="$(dirname "$CLIP")"
EMU="${EMU:-$ROOT/tools/emulator/slow32-fast}"
CA="${CLIPPER_SRC:-$HOME/refs/clipper/source}"
# Harbour runs in a container whose clock is UTC; the host's offset, as a
# POSIX TZ the container needs no zone files for, keeps DATE() the same
# day on both sides (a REPORT FORM header differed after 6 pm Mountain)
z=$(date +%z)
HB_TZ="LOC$([ "${z:0:1}" = "-" ] && echo + || echo -)${z:1:2}:${z:3:2}"
OUT="$CLIP/out/compat/ca"
mkdir -p "$OUT"
if [ ! -d "$CA/SAMPLE" ]; then echo "ca drivers: SKIP (no $CA)"; exit 0; fi
bash "$CLIP/build.sh" >/dev/null || exit 2
if [ $# -gt 0 ]; then DRIVERS=("$@"); else DRIVERS=("$HERE"/ca-*.prg); fi
pass=0; differ=0; nocomp=0; oracle=0
for drv in "${DRIVERS[@]}"; do
    n=$(basename "$drv" .prg)
    w=$(mktemp -d "$OUT/w.XXXX")
    srcs=$(head -1 "$drv" | sed -n 's/^\* CA: *//p')
    # The CA sources' preambles (what precedes their first FUNCTION or
    # PROCEDURE: comments, includes, file-wide STATICs) go above the
    # driver, where Clipper and Harbour want file-wide declarations; the
    # driver's own lines are the program's entry
    {
        for s in $srcs; do tr -d '\r' < "$CA/$s" | awk 'tolower($0) ~ /^[ \t]*(static[ \t]+)?(function|procedure|func|proc)[ \t]/ {exit} {print}'; done
        tr -d '\r' < "$drv"
        for s in $srcs; do echo; tr -d '\r' < "$CA/$s" | awk 'BEGIN{on=0} tolower($0) ~ /^[ \t]*(static[ \t]+)?(function|procedure|func|proc)[ \t]/ {on=1} on {print}'; done
    } > "$w/$n.prg"
    cp "$CA"/SAMPLE/*.CH "$CA"/SYS/*.CH "$w/" 2>/dev/null
    # a driver's data files, named after it (ca-x.frm beside ca-x.prg)
    for d in "$HERE/$n".*; do case "$d" in *.prg) ;; *) [ -f "$d" ] && cp "$d" "$w/";; esac; done
    for h in "$w"/*.CH; do mv "$h" "$w/$(basename "$h" | tr 'A-Z' 'a-z')"; done
    # our standard headers, under the spellings CA's sources use (Harbour's
    # file system is case-sensitive; ours finds them on its include path)
    for h in "$CLIP"/include/*.ch; do
        b=$(basename "$h"); cp "$h" "$w/$(echo "${b:0:1}" | tr 'a-z' 'A-Z')${b:1}"
    done
    podman run --rm -e TZ="$HB_TZ" -v "$w":/work slow32:harbour-oracle "$n.prg" > "$OUT/$n.hb" 2>&1
    rc=$?
    if [ $rc -eq 2 ] && grep -q ORACLE-COMPILE-FAIL "$OUT/$n.hb"; then
        printf '%-28s ORACLE (Harbour refuses it)\n' "$n"; oracle=$((oracle+1)); rm -rf "$w"; continue
    fi
    if ! bash "$CLIP/compile-prg.sh" "$w/$n.prg" "$OUT/$n.s32x" > "$OUT/$n.cc.log" 2>&1; then
        printf '%-28s NOCOMPILE %s\n' "$n" "$(grep -v '^  compile\|^$' "$OUT/$n.cc.log" | head -1 | cut -c1-60)"
        nocomp=$((nocomp+1)); rm -rf "$w"; continue
    fi
    (cd "$w" && timeout 30 "$EMU" -q --deny term "$OUT/$n.s32x" </dev/null > "$OUT/$n.clip" 2>&1)
    rm -rf "$w"
    if diff -q <(sed -e 's/[ \t]*$//' "$OUT/$n.hb") <(sed -e 's/[ \t]*$//' "$OUT/$n.clip") >/dev/null; then
        printf '%-28s PASS\n' "$n"; pass=$((pass+1))
    else
        printf '%-28s DIFFER (diff %s/%s.{hb,clip})\n' "$n" "$OUT" "$n"; differ=$((differ+1))
    fi
done
echo "ca drivers: $pass pass, $differ differ, $nocomp do not compile, $oracle refused by the oracle (of ${#DRIVERS[@]})"
[ $differ -eq 0 ]
