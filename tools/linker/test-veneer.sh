#!/bin/bash
# GitHub issue 74: a JAL past ±1MB must link via a veneer and still run.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
AS="$ROOT/tools/assembler/slow32asm"
LD="$ROOT/tools/linker/s32-ld"
EMU="${SELFHOST_EMU:-$ROOT/tools/dbt/slow32-dbt}"
W="$(mktemp -d /tmp/s32-veneer.XXXXXX)"
trap 'rm -rf "$W"' EXIT

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
"$LD" -o "$W/near.s32x" "$W/near.s32o"
rc="$("$EMU" "$W/near.s32x" >/dev/null; echo $?)"
[ "$rc" = 7 ] || { echo "near JAL: want rc=7 got $rc" >&2; exit 1; }

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
"$LD" --code-size 2M -o "$W/far.s32x" "$W/far.s32o" 2>"$W/ld.err"
grep -q 'JAL veneers' "$W/ld.err" || { echo "far JAL: expected veneer note" >&2; cat "$W/ld.err" >&2; exit 1; }
rc="$("$EMU" "$W/far.s32x" >/dev/null; echo $?)"
[ "$rc" = 42 ] || { echo "far JAL: want rc=42 got $rc" >&2; exit 1; }

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

echo "OK: JAL veneers (near rc=7, far rc=42)"
