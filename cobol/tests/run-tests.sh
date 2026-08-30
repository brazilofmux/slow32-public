#!/bin/bash
# cobol/ test harness.
#
# Gate 1 (pictest): pic_analyse over tests/pictures.txt against an expected
#   file checked by hand against the 1985 PICTURE clause text.
# Gate 2 (programs): every tests/fixed/*.cbl and tests/free/*.cbl compiled by
#   s32-cobc, assembled, linked with libcob and the SLOW-32 libc, run on the
#   emulator; stdout must match the .expected file.  The same source is also
#   compiled and run under GnuCOBOL and diffed, so the .expected files are
#   checked against the oracle as well as against us (docs/oracles.md:
#   -std=cobol85 for portable programs; a program whose first comment names
#   "default dialect" uses GnuCOBOL's default, because it exercises
#   implementor usages -std=cobol85 rejects).  GnuCOBOL is no longer
#   installed on any host: the oracle is the gnucobol:4.0-builder image
#   (cobc) and gnucobol:4.0-runtime (the built program), under podman or
#   docker, with the repo bind-mounted at its own path.  A host cobc, if
#   one exists, is used instead.  No oracle at all is reported, not hidden.
# Gate 3 (refusals): every tests/bad/*.cbl must be refused, and the message
#   must contain the text in its .expected file.  Unimplemented is a
#   diagnostic, never silence.
set -u

HERE="$(cd "$(dirname "$0")" && pwd)"
CDIR="$(cd "$HERE/.." && pwd)"
ROOT="$(cd "$CDIR/.." && pwd)"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
EMU="${EMU:-$ROOT/tools/emulator/slow32}"
COBC="$CDIR/out/s32-cobc"
LIBCOB="$CDIR/libcob/libcob.s32o"
# The oracle: host cobc if present, else GnuCOBOL in a container.
ORACLE_ENGINE=""
if command -v cobc >/dev/null 2>&1; then
    ORACLE_ENGINE=host
else
    for e in podman docker; do
        if command -v "$e" >/dev/null 2>&1 && "$e" image inspect gnucobol:4.0-builder >/dev/null 2>&1; then
            ORACLE_ENGINE="$e"
            ORACLE_RUN_IMAGE=gnucobol:4.0-builder
            "$e" image inspect gnucobol:4.0-runtime >/dev/null 2>&1 && ORACLE_RUN_IMAGE=gnucobol:4.0-runtime
            break
        fi
    done
fi

# The work directory lives under cobol/out (gitignored), not /tmp: a
# container engine on macOS can bind-mount the home directory but not
# /tmp, and the oracle compiles and runs inside the container on the
# same absolute paths the host sees.
mkdir -p "$CDIR/out"
W="$(mktemp -d "$CDIR/out/tests.XXXXXX")"
trap 'rm -rf "$W"' EXIT

oracle_cc() {   # oracle_cc out.orc [cobc args...]: compile under GnuCOBOL, cwd $W
    out="$1"; shift
    case "$ORACLE_ENGINE" in
        host) (cd "$W" && cobc -x "$@" -o "$out") ;;
        *)    "$ORACLE_ENGINE" run --rm -v "$ROOT:$ROOT" -w "$W" gnucobol:4.0-builder cobc -x "$@" -o "$out" ;;
    esac
}
oracle_run() {  # oracle_run prog.orc [args...]: run the oracle's program in $W/run,
                # standard input from $keys (the test's .keys file, or nothing)
    case "$ORACLE_ENGINE" in
        host) (cd "$W/run" && "$@" < "$keys") ;;
        *)    "$ORACLE_ENGINE" run --rm -i -v "$ROOT:$ROOT" -w "$W/run" "$ORACLE_RUN_IMAGE" "$@" < "$keys" ;;
    esac
}

PASS=0; FAIL=0

report() {
    if [ "$2" = "0" ]; then printf "  %-28s PASS%s\n" "$1:" "${3:+ ($3)}"; PASS=$((PASS+1))
    else printf "  %-28s FAIL%s\n" "$1:" "${3:+ ($3)}"; FAIL=$((FAIL+1)); fi
}

# Every program runs with a fresh copy of tests/data as its working
# directory (fixtures in data/, outputs to tmp/), for us and for the oracle.
fresh_workdir() {
    rm -rf "$W/run"; mkdir -p "$W/run/tmp"
    [ -d "$HERE/data" ] && cp -R "$HERE/data/." "$W/run/"
}

emu_run() {   # emu_run prog.s32x > stdout: the guest's output only
    # Everything between the emulator's "Starting execution" line and its
    # halt report is the program's.  Blank lines inside are the program's
    # too (DISPLAY of nothing), so this is a capture, not a grep -v.
    # The emulator writes one empty line of its own before "Program halted.";
    # hold each line back one step so that line can be dropped and a
    # program's own trailing blank line kept.
    # a .keys file beside the test is typed into the program (the term
    # service reads keys from the emulator's stdin)
    # a .args file beside the test is the program's command line
    (cd "$W/run" && "$EMU" "$1" $PROG_ARGS 2>/dev/null < "${2:-/dev/null}") | awk '
        /^Starting execution/ { capture = 1; held = 0; next }
        /^HALT at|^Program halted|^Exit code/ { if (held && prev != "") print prev; capture = 0; held = 0 }
        capture { if (held) print prev; prev = $0; held = 1 }
        END { if (held) print prev }'
}

if [ ! -x "$COBC" ] || [ ! -f "$LIBCOB" ]; then
    "$CDIR/build.sh" >/dev/null || { echo "build failed"; exit 1; }
fi

# --- Gate 1: PICTURE ---------------------------------------------------
if ! cc -std=c99 -I"$CDIR/src" -O1 -w -o "$W/pictest" "$HERE/pictest.c" \
        "$CDIR/src/picture.c" "$CDIR/src/picture_scan.c" 2>"$W/cc.log"; then
    report "pictest" 1 "host build"
else
    "$W/pictest" "$HERE/pictures.txt" > "$W/pictures.out" 2>&1
    if diff -q "$W/pictures.out" "$HERE/pictures.expected" >/dev/null 2>&1; then
        report "pictest" 0
    else
        report "pictest" 1 "mismatch"
        diff "$HERE/pictures.expected" "$W/pictures.out" | head -12
    fi
fi

# --- Gate 2: programs --------------------------------------------------
for fmt in fixed free; do
    for src in "$HERE/$fmt"/*.cbl; do
        [ -e "$src" ] || continue
        name="$(basename "$src" .cbl)"
        exp="${src%.cbl}.expected"
        flag="-$fmt"
        # a .link file beside the test names further sources (subprogram
        # .cbl, .c) relative to tests/, for us and for the oracle
        extra=()
        if [ -f "${src%.cbl}.link" ]; then
            for e in $(cat "${src%.cbl}.link"); do extra+=("$HERE/$e"); done
        fi
        if ! "$CDIR/compile.sh" $flag -I "$HERE/copy" "$src" "${extra[@]+"${extra[@]}"}" -o "$W/$name.s32x" >"$W/$name.log" 2>"$W/$name.err"; then
            report "$fmt/$name" 1 "$(grep -m1 -i "error" "$W/$name.err" "$W/$name.log" | head -1 | sed 's/^[^:]*://')"; continue
        fi
        fresh_workdir
        keys=/dev/null; [ -f "${src%.cbl}.keys" ] && keys="${src%.cbl}.keys"
        PROG_ARGS=""; [ -f "${src%.cbl}.args" ] && PROG_ARGS="$(cat "${src%.cbl}.args")"
        emu_run "$W/$name.s32x" "$keys" > "$W/$name.out"
        if [ ! -f "$exp" ]; then
            report "$fmt/$name" 1 "no .expected file"; continue
        fi
        if ! diff -q "$W/$name.out" "$exp" >/dev/null; then
            report "$fmt/$name" 1 "output mismatch"
            diff "$exp" "$W/$name.out" | head -8
            continue
        fi
        # a .tapemgr file lists "file maxlen" pairs the program wrote in
        # mode V: each goes through majesty's tapemgr (create a binary-V
        # dataset from it, extract it again) and must come back byte for
        # byte -- the RDW on disk is IBM's, not a private length word
        if [ -f "${src%.cbl}.tapemgr" ] && [ -x "$HOME/majesty/tapemgr" ]; then
            tm_ok=1
            while read -r vf vlen; do
                [ -n "$vf" ] || continue
                cat > "$W/tm.json" <<JSON
{ "volume_serial": "S32V01", "owner_code": "SLOW32", "files": [ { "dataset_name": "S32.VREC", "local_file": "$W/run/$vf", "record_format": "V", "record_length": $vlen, "block_size": 4096, "binary": true } ] }
JSON
                cat > "$W/tmx.json" <<JSON
{ "volume_serial": "S32V01", "owner_code": "SLOW32", "files": [ { "dataset_name": "S32.VREC", "local_file": "$W/tm-back.dat", "record_format": "V", "record_length": $vlen, "block_size": 4096, "binary": true } ] }
JSON
                rm -f "$W/tm.aws" "$W/tm-back.dat"
                # (tapemgr create drops a RESTORE.JCL in its cwd; keep that in $W)
                if ! (cd "$W" && "$HOME/majesty/tapemgr" create --volser=S32V01 -o "$W/tm.aws" -c "$W/tm.json") >"$W/tm.log" 2>&1 ||
                   ! (cd "$W" && "$HOME/majesty/tapemgr" extract -c "$W/tmx.json" "$W/tm.aws") >>"$W/tm.log" 2>&1 ||
                   ! cmp -s "$W/run/$vf" "$W/tm-back.dat"; then
                    tm_ok=0; report "$fmt/$name" 1 "tapemgr round trip of $vf failed: $(tail -1 "$W/tm.log")"; break
                fi
            done < "${src%.cbl}.tapemgr"
            [ "$tm_ok" = 1 ] || continue
        fi
        # oracle: GnuCOBOL on the same source, when present.  A program whose
        # comments say "no oracle" (screens need a tty there) is ours alone.
        note=""
        if grep -qi "no oracle" "$src"; then note="no oracle: reviewed by hand"; ORACLE_SKIP=1; else ORACLE_SKIP=0; fi
        if [ -n "$ORACLE_ENGINE" ] && [ "$ORACLE_SKIP" = 0 ]; then
            std="-std=cobol85"
            grep -qi "default dialect" "$src" && std=""
            if oracle_cc "$W/$name.orc" $std $flag -I "$HERE/copy" "$src" "${extra[@]+"${extra[@]}"}" >"$W/$name.orclog" 2>&1; then
                fresh_workdir
                oracle_run "$W/$name.orc" $PROG_ARGS > "$W/$name.orcout" 2>/dev/null
                # a documented divergence from GnuCOBOL (docs/oracles.md) keeps
                # GnuCOBOL's own output beside the standard's in .oracle-expected
                oexp="$exp"; [ -f "${src%.cbl}.oracle-expected" ] && oexp="${src%.cbl}.oracle-expected"
                if diff -q "$W/$name.orcout" "$oexp" >/dev/null; then
                    note="oracle agrees"; [ "$oexp" != "$exp" ] && note="oracle agrees with its documented divergence"
                    [ -f "${src%.cbl}.tapemgr" ] && note="$note; tapemgr round trip"
                else
                    report "$fmt/$name" 1 "GnuCOBOL disagrees with .expected"
                    diff "$exp" "$W/$name.orcout" | head -8
                    continue
                fi
            else
                note="oracle refused it: $(grep -m1 error "$W/$name.orclog" | cut -c1-60)"
            fi
        fi
        report "$fmt/$name" 0 "$note"
    done
done

# --- Gate 3: refusals --------------------------------------------------
for src in "$HERE/bad"/*.cbl; do
    [ -e "$src" ] || continue
    name="$(basename "$src" .cbl)"
    exp="${src%.cbl}.expected"
    flag="-fixed"; grep -q "^identification division" "$src" && flag="-free"
    [ "$name" = "mixed-format" ] && flag="-fixed"
    if "$COBC" $flag -I "$HERE/copy" -o "$W/$name.s" "$src" 2>"$W/$name.err"; then
        report "bad/$name" 1 "was accepted"; continue
    fi
    if grep -qF "$(cat "$exp")" "$W/$name.err"; then
        report "bad/$name" 0 "$(cut -d: -f3- "$W/$name.err" | head -1 | cut -c1-50)"
    else
        report "bad/$name" 1 "wrong message: $(head -1 "$W/$name.err")"
    fi
done

echo
case "$ORACLE_ENGINE" in
    "")   echo "cobol: NO ORACLE -- neither a host cobc nor a gnucobol:4.0-builder image; .expected files were checked against us alone" ;;
    host) echo "cobol: oracle is the host cobc" ;;
    *)    echo "cobol: oracle is gnucobol:4.0-builder / $ORACLE_RUN_IMAGE under $ORACLE_ENGINE" ;;
esac
echo "cobol: $PASS passed, $FAIL failed"
[ "$FAIL" = "0" ]
