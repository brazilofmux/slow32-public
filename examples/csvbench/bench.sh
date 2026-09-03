#!/bin/bash
# Criterion 3: does distributing the CSV validation across N slow32 worker
# instances beat one serial instance over all files?  Serial baseline vs the
# cluster launcher at several worker counts.  Run `./gen.sh` first.
cd "$(dirname "$0")"
SER=../validatecsv_serial.s32x
EMU="${EMU:-../../tools/emulator/slow32-fast}"
FILES=(corpus/*.csv)
[ -e "${FILES[0]}" ] || { echo "no corpus; run ./gen.sh"; exit 1; }
now(){ python3 -c 'import time;print(time.time())'; }
minof(){ local n="$1"; shift; local best=; for i in $(seq 1 "$n"); do
  local s e t; s=$(now); "$@" >/dev/null 2>&1; e=$(now); t=$(python3 -c "print($e-$s)")
  if [ -z "$best" ] || python3 -c "exit(0 if $t<$best else 1)"; then best=$t; fi
done; python3 -c "print(f'{$best*1000:.1f}')"; }
nf=${#FILES[@]}
echo "corpus: $nf files, $(du -sh corpus | cut -f1)"
ser=$(minof 5 "$EMU" "$SER" "${FILES[@]}")
printf "%-16s %8s ms   %5s   %s\n" "serial (1)" "$ser" "1.00x" "-"
for J in 2 4 8 18; do
  t=$(minof 5 env EMU="$EMU" BIN="$SER" bash cluster.sh -j "$J" "${FILES[@]}")
  sp=$(python3 -c "print(f'{$ser/$t:.2f}x')")
  eff=$(python3 -c "print(f'{100*($ser/$t)/$J:.0f}%')")
  printf "%-16s %8s ms   %5s   eff %s\n" "cluster -j $J" "$t" "$sp" "$eff"
done
