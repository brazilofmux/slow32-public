#!/usr/bin/env python3
"""An emulator stand-in for a script that runs many programs (majesty's
batch.sh takes S32_EMU): the same arguments, and a measurement per run.

  S32_EMU=bench/emu-standin.py MODE=time  LOG=f  ./batch.sh
      run under the DBT (REAL_EMU, default tools/dbt/slow32-dbt); LOG gets
      "program ms", the DBT's process alone timed
  MODE=count LOG=f
      run under slow32-fast; LOG gets "program instructions"
  MODE=prof TARGET=name BIN=prog-p.s32x OUT=prog.prof
      the program TARGET.s32x is run as the reference interpreter's
      `slow32 -p OUT BIN args` in the same directory (BIN: the program
      built against bench/prof.sh's labelled libcob); every other program
      runs under the DBT.  Then bench/prof.py BIN OUT ...

The program's own output passes through; the emulators' banners and
statistics do not.  docs/performance.md 2026-10-08 has the method."""
import os, re, subprocess, sys, time
args = sys.argv[1:]
prog = next((a for a in args if a.endswith(".s32x")), args[0] if args else "?")
mode = os.environ.get("MODE", "time")
root = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..")
dbt = os.environ.get("REAL_EMU", os.path.join(root, "tools", "dbt", "slow32-dbt"))
def log(s):
    f = os.environ.get("LOG")
    if f:
        with open(f, "a") as h: h.write("%s %s\n" % (os.path.basename(prog), s))
if mode == "prof" and os.path.basename(prog) == os.environ["TARGET"] + ".s32x":
    rest = [a for a in args if a != prog]
    r = subprocess.run([os.path.join(root, "tools", "emulator", "slow32"), "-p", os.environ["OUT"], os.environ["BIN"]] + rest, capture_output=True, text=True)
    sys.stdout.write(re.sub(r"(?s)\nStarting execution.*", "\n", r.stdout) if "Starting execution" in r.stdout else r.stdout)
    sys.exit(r.returncode)
if mode == "count":
    r = subprocess.run([os.path.join(root, "tools", "emulator", "slow32-fast")] + args, capture_output=True, text=True)
    m = re.search(r"Instructions executed:\s*(\d+)", r.stdout + r.stderr)
    log(m.group(1) if m else "?")
    sys.stdout.write(re.sub(r"(?s)\nStarting execution.*", "\n", r.stdout) if "Starting execution" in r.stdout else r.stdout)
    sys.exit(r.returncode)
t0 = time.perf_counter_ns()
rc = subprocess.call([dbt] + args)
log("%.1f" % ((time.perf_counter_ns() - t0) / 1e6))
sys.exit(rc)
