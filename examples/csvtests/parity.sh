#!/bin/bash
# Byte-for-byte parity: serial baseline vs scheduler version.
EMU=../../tools/emulator/slow32-fast
SER=../validatecsv_serial.s32x
SCH=../validatecsv_sched.s32x
cd "$(dirname "$0")"
strip() { grep -vE '^(Starting execution|MMIO enabled|HALT at|Program halted|Exit code:|Instructions executed|Simulated cycles|Wall time:|Performance:)' | grep -v 'instructions/second' | grep -vE '^[[:space:]]*$' ; }
run() { local bin="$1"; shift; "$EMU" "$bin" "$@" 2>/dev/null | strip; }
fail=0
check() { # check <label> <args...>
  local label="$1"; shift
  local so sc rs rc
  so="$(run "$SER" "$@")"; rs=${PIPESTATUS[0]}
  # capture exit code separately
  "$EMU" "$SER" "$@" >/dev/null 2>&1; rs=$?
  sc="$(run "$SCH" "$@")"
  "$EMU" "$SCH" "$@" >/dev/null 2>&1; rc=$?
  if [ "$so" = "$sc" ] && [ "$rs" = "$rc" ]; then
    echo "  OK  $label (exit=$rs)"
  else
    echo "  FAIL $label"
    echo "  -- serial (exit=$rs) --"; echo "$so" | sed 's/^/    /'
    echo "  -- sched  (exit=$rc) --"; echo "$sc" | sed 's/^/    /'
    fail=1
  fi
}
FILES="valid.csv valid_noeol.csv fieldcount.csv crlf.csv badcr.csv quoted.csv empty.csv unquoted_quote.csv"
for f in $FILES; do check "loose $f" "$f"; done
check "strict unquoted_quote" -s unquoted_quote.csv
check "strict valid" -s valid.csv
check "multi loose (all, order+batch=8)" $FILES
check "strict multi" -s $FILES
# >8 files to force a second batch
check "multi >8 (two batches)" valid.csv fieldcount.csv valid.csv badcr.csv quoted.csv empty.csv crlf.csv valid_noeol.csv unquoted_quote.csv valid.csv
check "no files (usage)"
[ $fail -eq 0 ] && echo "=== PARITY OK ===" || echo "=== PARITY FAILED ==="
exit $fail
