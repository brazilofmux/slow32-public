#!/bin/bash
# Build ppcs32, the SLOW-32 cross-compiler, in two stages on this Mac.
#
# Stage 2: the installed FPC 3.2.2 (brew, aarch64-darwin) builds a trunk
#          compiler for the host, ppca64.trunk, plus the trunk RTL for
#          aarch64-darwin it needs (rtl/units/aarch64-darwin).
# Stage 3: ppca64.trunk builds ppcs32.
#
# Why not one stage: 3.2.2 on aarch64-darwin miscompiles trunk's compiler
# sources.  A ppcs32 built by it directly crashed with "Compilation raised
# exception internally" on `with r^ do begin ... exit end` (node_complexity
# read a freed tempinfo: tblocknode.simplify's strip-after-exit had freed
# the tempdelete node because has_node_of_type gave wrong answers), died
# with EBusError compiling varutils.pp, and could not compile variants.pp
# or rtti.pp.  The same sources built by the trunk host compiler have none
# of this (2026-10-10, found by the FPC test suite run).
#
# Usage: build-compiler.sh [--stage2-only]   env: FPC_SRC (~/fpc), HOST_FPC
set -euo pipefail
FPC_SRC="${FPC_SRC:-$HOME/fpc}"
HOST_FPC="${HOST_FPC:-/opt/homebrew/lib/fpc/3.2.2/ppca64}"
HOST_RTL="${HOST_RTL:-/opt/homebrew/lib/fpc/3.2.2/units/aarch64-darwin/rtl}"
export PPC_CONFIG_PATH="${PPC_CONFIG_PATH:-/opt/homebrew/etc}"
SDK="$(xcrun --show-sdk-path)"
WORK="${WORK:-$FPC_SRC/compiler/bootstrap}"
mkdir -p "$WORK/a64units" "$WORK/s32units"
cd "$FPC_SRC/compiler"
# msgidx.inc and msgtxt.inc come from msg/errore.msg, as `make msg` (and
# Dockerfile.toolchain) make them: upstream commits a new message to
# errore.msg ahead of the regenerated files at times (2026-10-20:
# sym_e_type_must_be_record, "Identifier not found" in pdecvar.pas).
echo "stage 1: message files from msg/errore.msg"
"$HOST_FPC" -Fu"$HOST_RTL" -FE"$WORK" utils/msg2inc.pp > "$WORK/stage1.log" 2>&1 \
    || { tail -5 "$WORK/stage1.log"; exit 1; }
"$WORK/msg2inc" msg/errore.msg msg msg >> "$WORK/stage1.log" 2>&1 \
    || { tail -5 "$WORK/stage1.log"; exit 1; }
echo "stage 2: trunk ppca64 (host) with $HOST_FPC"
"$HOST_FPC" -B -daarch64 -Fuaarch64 -Fuarmgen -Fusystems -Fiaarch64 -Fiarmgen \
    -Fu"$HOST_RTL" -FU"$WORK/a64units" -o"$WORK/ppca64.trunk" pp.pas > "$WORK/stage2.log" 2>&1 \
    || { tail -5 "$WORK/stage2.log"; exit 1; }
echo "stage 2: trunk RTL for aarch64-darwin"
make -C "$FPC_SRC/rtl/darwin" FPC="$WORK/ppca64.trunk" CPU_TARGET=aarch64 OS_TARGET=darwin OPT="-n" > "$WORK/stage2-rtl.log" 2>&1 \
    || { tail -5 "$WORK/stage2-rtl.log"; exit 1; }
[[ "${1:-}" == "--stage2-only" ]] && exit 0
echo "stage 3: ppcs32 with ppca64.trunk"
"$WORK/ppca64.trunk" -B -n -dslow32 -Fuslow32 -Fusystems -Fislow32 \
    -Fu"$FPC_SRC/rtl/units/aarch64-darwin" -FU"$WORK/s32units" -Fl"$SDK/usr/lib" -XR"$SDK" \
    -oppcs32 pp.pas > "$WORK/stage3.log" 2>&1 \
    || { tail -5 "$WORK/stage3.log"; exit 1; }
echo "built $FPC_SRC/compiler/ppcs32: $(wc -c < "$FPC_SRC/compiler/ppcs32") bytes"
