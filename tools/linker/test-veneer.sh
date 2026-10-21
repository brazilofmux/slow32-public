#!/bin/bash
# GitHub issue 74 / 85: a JAL past ±1MB must link via a veneer and still run.
# Default: host s32-ld.  A second pass runs the same cases (except -r) against
# the kit linker: KIT_LD if set, else a host-built port of
# selfhost/stage08/tools/s32-ld.c, else selfhost/stage08/s32-ld.s32x via the
# emulator.  The kit has no relocatable output, so VENEER_SKIP_R skips -r.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
AS="$ROOT/tools/assembler/slow32asm"
HOST_LD="$ROOT/tools/linker/s32-ld"
EMU="${SELFHOST_EMU:-$ROOT/tools/dbt/slow32-dbt}"
W="$(mktemp -d /tmp/s32-veneer.XXXXXX)"
trap 'rm -rf "$W"' EXIT

# $1 = linker command (evaluated), $2 = 1 to skip -r, $3 = tag for messages
run_veneer_cases() {
    local LD_CMD="$1"
    local SKIP_R="$2"
    local tag="$3"
    local rc

    cat > "$W/near.s" <<'EOF'
.global _start
_start:
    jal r31, near
    halt
near:
    addi r1, r0, 7
    jalr r0, r31, 0
EOF
    "$AS" "$W/near.s" "$W/near.s32o" >/dev/null
    eval "$LD_CMD" -o "$W/near.s32x" "$W/near.s32o" >/dev/null 2>"$W/near.err"
    rc="$("$EMU" "$W/near.s32x" >/dev/null; echo $?)"
    [ "$rc" = 7 ] || { echo "$tag near JAL: want rc=7 got $rc" >&2; cat "$W/near.err" >&2; exit 1; }

    cat > "$W/far.s" <<'EOF'
.global _start
_start:
    jal r31, far
    halt
.space 0x100010
far:
    addi r1, r0, 42
    jalr r0, r31, 0
EOF
    "$AS" "$W/far.s" "$W/far.s32o" >/dev/null
    eval "$LD_CMD" --code-size 2M -o "$W/far.s32x" "$W/far.s32o" >/dev/null 2>"$W/ld.err"
    grep -q 'JAL veneers' "$W/ld.err" || { echo "$tag far JAL: expected veneer note" >&2; cat "$W/ld.err" >&2; exit 1; }
    rc="$("$EMU" "$W/far.s32x" >/dev/null; echo $?)"
    [ "$rc" = 42 ] || { echo "$tag far JAL: want rc=42 got $rc" >&2; exit 1; }

    # Interior islands (ACAS: 115 COBOL programs, more text than the start and
    # end islands cover): a call from the middle of three units, over 1MB
    # from both ends, reaches its target through an island between units.
    cat > "$W/u1.s" <<'EOF'
.global _start
.global f1
_start:
    jal r31, mid
    halt
f1:
    addi r1, r0, 55
    jalr r0, r31, 0
.space 0xE0000
EOF
    cat > "$W/u2.s" <<'EOF'
.global mid
.space 0x60000
mid:
    add r20, r31, r0
    jal r31, f1
    jalr r0, r20, 0
.space 0xE0000
EOF
    cat > "$W/u3.s" <<'EOF'
.space 0xE0000
EOF
    for u in u1 u2 u3; do "$AS" "$W/$u.s" "$W/$u.s32o" >/dev/null; done
    eval "$LD_CMD" --code-size 4M -o "$W/mid.s32x" "$W/u1.s32o" "$W/u2.s32o" "$W/u3.s32o" >/dev/null 2>"$W/ld3.err" ||
        { echo "$tag interior island: link failed" >&2; cat "$W/ld3.err" >&2; exit 1; }
    rc="$("$EMU" "$W/mid.s32x" >/dev/null; echo $?)"
    [ "$rc" = 55 ] || { echo "$tag interior island: want rc=55 got $rc" >&2; exit 1; }

    if [ "$SKIP_R" != 1 ]; then
        # The same three units partially linked first (s32-ld -r, a module as
        # TinyMUX's static build makes them): islands go only between parts, and
        # the -r output merged the three into one, leaving the call from the middle
        # with nowhere to reach.  It keeps one code section per part now.
        eval "$LD_CMD" -r -o "$W/mod3.s32o" "$W/u1.s32o" "$W/u2.s32o" "$W/u3.s32o" >/dev/null
        eval "$LD_CMD" --code-size 4M -o "$W/modmid.s32x" "$W/mod3.s32o" >/dev/null 2>"$W/ld4.err" ||
            { echo "$tag interior island through -r: link failed" >&2; cat "$W/ld4.err" >&2; exit 1; }
        rc="$("$EMU" "$W/modmid.s32x" >/dev/null; echo $?)"
        [ "$rc" = 55 ] || { echo "$tag interior island through -r: want rc=55 got $rc" >&2; exit 1; }
    fi

    # Prepend used to skip symbols sitting exactly at heap_base (__heap_start),
    # so malloc's arena started in .data.  The s32x header's heap_base and the
    # __heap_start symbol must stay in lockstep.
    python3 - "$W/far.s32x" <<'PY'
import struct, sys
b = open(sys.argv[1], "rb").read()
magic, ver, endian, machine, entry, nsec, sec_off, str_off, str_sz, flags, \
    code_limit, rodata_limit, data_limit, stack_base, mem_size, heap_base, \
    stack_end, mmio_base = struct.unpack_from("<IHBBIIIIIIIIIIIIII", b, 0)
names = b[str_off:str_off + str_sz]
secs = {}
for i in range(nsec):
    no, typ, vaddr, off, size, mem, fl = struct.unpack_from("<IIIIIII", b, sec_off + i * 28)
    secs[names[no:names.find(b"\0", no)].decode()] = (off, size)
if ".symtab" not in secs or ".sym_strtab" not in secs:
    sys.exit("missing symtab")
sym_off, sym_size = secs[".symtab"]
str_off, str_size = secs[".sym_strtab"]
sstr = b[str_off:str_off + str_size]
for k in range(sym_size // 16):
    no, val, sec, typ, bind, sz = struct.unpack_from("<IIHBBI", b, sym_off + k * 16)
    nm = sstr[no:sstr.find(b"\0", no)].decode()
    if nm == "__heap_start":
        if val != heap_base:
            print(f"__heap_start {val:#x} != header heap_base {heap_base:#x}",
                  file=sys.stderr)
            sys.exit(1)
        print(f"heap lockstep {val:#x}")
        sys.exit(0)
sys.exit("no __heap_start")
PY

    if [ "$SKIP_R" = 1 ]; then
        echo "OK: JAL veneers ($tag: near rc=7, far rc=42, interior rc=55)"
    else
        echo "OK: JAL veneers ($tag: near rc=7, far rc=42, interior rc=55, through -r rc=55)"
    fi
}

run_veneer_cases "$HOST_LD" 0 host

# Kit linker: prefer KIT_LD, else host-gcc of the cc-min source (fast, same
# code the s32x is compiled from), else the built s32-ld.s32x under the emu.
KIT_SRC="$ROOT/selfhost/stage08/tools/s32-ld.c"
KIT_S32X="$ROOT/selfhost/stage08/s32-ld.s32x"
if [ -n "${KIT_LD:-}" ]; then
    run_veneer_cases "$KIT_LD" 1 kit
elif gcc -O0 -Wno-builtin-declaration-mismatch -o "$W/s32-ld-kit" "$KIT_SRC" 2>/dev/null; then
    run_veneer_cases "$W/s32-ld-kit" 1 kit-ccmin
elif [ -f "$KIT_S32X" ]; then
    run_veneer_cases "$EMU $KIT_S32X" 1 kit-s32x
else
    echo "skip: kit linker (no KIT_LD, gcc failed, no $KIT_S32X)" >&2
fi
