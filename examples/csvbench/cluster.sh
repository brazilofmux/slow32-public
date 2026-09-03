#!/bin/bash
# The desk file: split files across N slow32 worker instances, run them in
# parallel, merge their output in file order.  The host composes the machines;
# each worker is a serial validatecsv over a contiguous slice of the list.
#   cluster.sh -j N [-s] file...
EMU="${EMU:-../../tools/emulator/slow32-fast}"
BIN="${BIN:-../validatecsv_serial.s32x}"
cd "$(dirname "$0")"
J=4; SFLAG=(); files=()
while [ $# -gt 0 ]; do
  case "$1" in
    -j) J="$2"; shift 2;;
    -s) SFLAG=(-s); shift;;
    *) files+=("$1"); shift;;
  esac
done
nf=${#files[@]}
[ "$nf" -eq 0 ] && { echo "no files"; exit 2; }
[ "$J" -gt "$nf" ] && J="$nf"
tmp="$(mktemp -d)"
per=$(( (nf + J - 1) / J ))
pids=(); g=0
for ((start=0; start<nf; start+=per)); do
  slice=( "${files[@]:start:per}" )
  "$EMU" "$BIN" "${SFLAG[@]}" "${slice[@]}" >"$tmp/out.$g" 2>/dev/null &
  pids+=($!); g=$((g+1))
done
rc=0
for ((i=0;i<${#pids[@]};i++)); do wait "${pids[$i]}" || rc=1; done
# merge in group order (which is file order), strip emulator banner
for ((i=0;i<g;i++)); do
  grep -vE '^(Starting execution|MMIO enabled|HALT at|Program halted|Exit code:|Instructions executed|Simulated cycles|Wall time:|Performance:)' "$tmp/out.$i" | grep -v 'instructions/second' | grep -vE '^[[:space:]]*$'
done
rm -rf "$tmp"
exit $rc
