#!/usr/bin/env python3
"""
Quick-and-dirty harness to compare slow32-fast vs. qemu-system-slow32.

Uses host wall-clock timing. Optional guest stats lines (if the backend
prints them) are still parsed when present, but are no longer required.

Example:
    python3 scripts/slow32/compare.py --image /path/to/hello.s32x
    python3 scripts/slow32/compare.py --suite ~/slow-32/examples
"""

from __future__ import annotations

import argparse
import os
import pathlib
import re
import subprocess
import sys
import time
from dataclasses import dataclass
from typing import List, Optional, Sequence, Tuple


ROOT = pathlib.Path(__file__).resolve().parents[2]
DEFAULT_IMAGE = "/tmp/s32_smoke.s32x"
QEMU_BIN = ROOT / "build" / "qemu-system-slow32"

# Prefer a sibling slow-32 checkout (common local layout), then $SLOW32_ROOT,
# then the historical in-tree path that older checkouts used.
def _default_fast_bin() -> pathlib.Path:
    candidates = [
        pathlib.Path(os.environ["SLOW32_ROOT"]) / "tools" / "emulator" / "slow32-fast"
        if "SLOW32_ROOT" in os.environ
        else None,
        pathlib.Path.home() / "slow-32" / "tools" / "emulator" / "slow32-fast",
        ROOT.parent / "slow-32" / "tools" / "emulator" / "slow32-fast",
        ROOT / "slow-32" / "tools" / "emulator" / "slow32-fast",
    ]
    for path in candidates:
        if path is not None and path.exists():
            return path
    return pathlib.Path.home() / "slow-32" / "tools" / "emulator" / "slow32-fast"


FAST_BIN = _default_fast_bin()

# Optional: only present when the backend is built with stats instrumentation.
SLOW32_STATS_RE = re.compile(
    r"Slow32 stats: guest_insns=(?P<insns>\d+)\s+"
    r"wall_ms=(?P<wall>[0-9.]+)\s+"
    r"translate_ms=(?P<translate>[0-9.]+)\s+"
    r"exec_ms=(?P<exec>[0-9.]+)\s+tb_count=(?P<tbs>\d+)"
)
FAST_STATS_RE = re.compile(r"Instructions executed:\s+(?P<insns>\d+)")
FAST_TIME_RE = re.compile(r"Wall time:\s+(?P<secs>[0-9.]+)\s+seconds")


@dataclass
class CompareResult:
    image: str
    iterations: int
    qemu_wall: float
    qemu_translate: Optional[float]
    qemu_exec: Optional[float]
    qemu_tb_count: Optional[float]
    qemu_insns: Optional[float]
    qemu_exit: int
    fast_wall: Optional[float]
    fast_insns: Optional[float]
    fast_exit: int

    @property
    def abnormal(self) -> bool:
        return self.qemu_exit != 0 or self.fast_exit != 0

    @property
    def speed_ratio(self) -> Optional[float]:
        if self.fast_wall is None or self.qemu_wall <= 0:
            return None
        return self.fast_wall / self.qemu_wall

    @property
    def translate_fraction(self) -> Optional[float]:
        if self.qemu_translate is None or self.qemu_wall <= 0:
            return None
        return min(1.0, max(0.0, self.qemu_translate / self.qemu_wall))


def warn_abnormal(name: str, cmd: Sequence[str], returncode: int,
                  stderr: str, iteration: int) -> None:
    """A nonzero exit means the timing row is suspect; say so loudly."""
    sys.stderr.write(
        f"WARNING: {name} exited {returncode} (iteration {iteration}); "
        "timings for this workload are not meaningful\n"
        f"  command: {' '.join(cmd)}\n"
    )
    tail = [ln for ln in stderr.splitlines() if ln.strip()][-5:]
    for line in tail:
        sys.stderr.write(f"  stderr: {line}\n")


def run_cmd(argv: Sequence[str]) -> Tuple[float, subprocess.CompletedProcess[str]]:
    start = time.perf_counter()
    proc = subprocess.run(
        argv,
        text=True,
        encoding="utf-8",
        errors="replace",
        capture_output=True,
    )
    duration = time.perf_counter() - start
    return duration, proc


def ensure_executable(path: pathlib.Path) -> None:
    if not path.exists():
        sys.exit(f"error: {path} does not exist (build slow32-fast / qemu first)")
    if not os.access(path, os.X_OK):
        sys.exit(f"error: {path} is not executable")


def collect_images(default_image: str, images: Sequence[str],
                   suite_paths: Sequence[str]) -> List[str]:
    ordered: List[str] = []
    seen = set()

    def add(path: str) -> None:
        abspath = os.path.abspath(path)
        if abspath not in seen:
            ordered.append(abspath)
            seen.add(abspath)

    for image in images:
        add(image)

    for entry in suite_paths:
        path = pathlib.Path(entry)
        if path.is_dir():
            for candidate in sorted(path.glob("*.s32x")):
                add(str(candidate))
            continue
        if path.is_file():
            # A bare .s32x is a workload, not a text manifest.
            if path.suffix == ".s32x":
                add(str(path))
                continue
            for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                add(line)
            continue
        sys.exit(f"error: suite source '{entry}' does not exist")

    if not ordered:
        add(default_image)

    return ordered


def run_qemu(cmd: Sequence[str], iterations: int
             ) -> Tuple[float, Optional[float], Optional[float], Optional[float],
                        Optional[float], int]:
    """Return (wall, insns, translate, exec, tbs, last_exit)."""
    wall_times: List[float] = []
    insns: List[float] = []
    translate: List[float] = []
    exec_times: List[float] = []
    tb_counts: List[float] = []
    last_exit = 0

    for i in range(iterations):
        host_wall, proc = run_cmd(cmd)
        if proc.returncode < 0:
            sys.stderr.write(proc.stdout)
            sys.stderr.write(proc.stderr)
            sys.exit(f"slow32-tcg terminated by signal (iteration {i + 1})")
        if proc.returncode != 0:
            warn_abnormal("qemu-system-slow32", cmd, proc.returncode,
                          proc.stderr, i + 1)
        last_exit = proc.returncode
        match = SLOW32_STATS_RE.search(proc.stdout)
        if match:
            # Prefer guest-reported wall when instrumentation is present.
            wall_times.append(float(match.group("wall")) / 1000.0)
            insns.append(int(match.group("insns")))
            translate.append(float(match.group("translate")) / 1000.0)
            exec_times.append(float(match.group("exec")) / 1000.0)
            tb_counts.append(int(match.group("tbs")))
        else:
            wall_times.append(host_wall)

    def avg(xs: List[float]) -> Optional[float]:
        return sum(xs) / len(xs) if xs else None

    return (
        sum(wall_times) / len(wall_times),
        avg(insns),
        avg(translate),
        avg(exec_times),
        avg(tb_counts),
        last_exit,
    )


def run_fast(cmd: Sequence[str], iterations: int
             ) -> Tuple[Optional[float], Optional[float], int]:
    wall_times: List[float] = []
    insns: List[float] = []
    last_exit = 0
    for i in range(iterations):
        host_wall, proc = run_cmd(cmd)
        if proc.returncode < 0:
            sys.stderr.write(proc.stdout)
            sys.stderr.write(proc.stderr)
            sys.exit(f"slow32-fast terminated by signal (iteration {i + 1})")
        if proc.returncode != 0:
            warn_abnormal("slow32-fast", cmd, proc.returncode,
                          proc.stderr, i + 1)
        last_exit = proc.returncode
        m_insn = FAST_STATS_RE.search(proc.stdout)
        if m_insn:
            insns.append(int(m_insn.group("insns")))
        m_time = FAST_TIME_RE.search(proc.stdout)
        if m_time:
            wall_times.append(float(m_time.group("secs")))
        else:
            wall_times.append(host_wall)
    avg_time = sum(wall_times) / len(wall_times) if wall_times else None
    avg_insn = sum(insns) / len(insns) if insns else None
    return avg_time, avg_insn, last_exit


def compare_image(image: str, iterations: int, fast_bin: str,
                  qemu_bin: str) -> CompareResult:
    fast_cmd = [fast_bin, image]
    qemu_cmd = [
        qemu_bin,
        "-machine",
        "slow32-tcg",
        "-kernel",
        image,
        "-display",
        "none",
        "-monitor",
        "none",
    ]

    fast_time, guest_insns, fast_exit = run_fast(fast_cmd, iterations)
    qemu_time, qemu_insns, qemu_translate, qemu_exec, qemu_tbs, qemu_exit = run_qemu(
        qemu_cmd, iterations
    )

    return CompareResult(
        image=image,
        iterations=iterations,
        qemu_wall=qemu_time,
        qemu_translate=qemu_translate,
        qemu_exec=qemu_exec,
        qemu_tb_count=qemu_tbs,
        qemu_insns=qemu_insns,
        qemu_exit=qemu_exit,
        fast_wall=fast_time,
        fast_insns=guest_insns,
        fast_exit=fast_exit,
    )


def print_single(result: CompareResult) -> None:
    print("slow32 comparison (lower is better)")
    print("-----------------------------------")
    print(f" image: {result.image}")
    print(f" iterations: {result.iterations}")
    print()
    if result.qemu_translate is not None and result.qemu_exec is not None:
        tb = int(result.qemu_tb_count or 0)
        print(
            " qemu-system-slow32: "
            f"{result.qemu_wall:.6f}s (translate {result.qemu_translate:.6f}s, "
            f"exec {result.qemu_exec:.6f}s, tb_count≈{tb})"
        )
    else:
        print(f" qemu-system-slow32: {result.qemu_wall:.6f}s (host wall clock)")
    if result.fast_wall is not None:
        print(f" slow32-fast:        {result.fast_wall:.6f}s")
    else:
        print(" slow32-fast:        <no wall clock available>")
    if result.fast_insns is not None or result.qemu_insns is not None:
        fast_i = int(result.fast_insns) if result.fast_insns is not None else "?"
        qemu_i = int(result.qemu_insns) if result.qemu_insns is not None else "n/a"
        print(f" guest instructions: {fast_i} (qemu reported {qemu_i})")
    print(f" exit codes:         fast={result.fast_exit} qemu={result.qemu_exit}")
    ratio = result.speed_ratio
    if ratio is not None:
        print(f" speed ratio (fast/tcg): {ratio:.3f}x")
    frac = result.translate_fraction
    if frac is not None:
        print(
            f" translate share: {frac * 100:.1f}% "
            "(relative to total qemu time)"
        )


def print_table(results: Sequence[CompareResult]) -> None:
    header = (
        f"{'image':28} {'wall (s)':>10} {'fast (s)':>10} "
        f"{'ratio':>8} {'exit f/q':>10}"
    )
    print(header)
    print("-" * len(header))
    for res in results:
        ratio = res.speed_ratio
        ratio_str = f"{ratio:.3f}" if ratio is not None else "n/a"
        fast_wall = res.fast_wall if res.fast_wall is not None else 0.0
        print(
            f"{pathlib.Path(res.image).name:28} "
            f"{res.qemu_wall:10.6f} "
            f"{fast_wall:10.6f} {ratio_str:>8} "
            f"{f'{res.fast_exit}/{res.qemu_exit}':>10}"
        )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--image",
        default=DEFAULT_IMAGE,
        help=f"path to .s32x binary (default: {DEFAULT_IMAGE})",
    )
    parser.add_argument(
        "images",
        nargs="*",
        help="additional .s32x images to compare (can also be provided via --suite)",
    )
    parser.add_argument(
        "--suite",
        action="append",
        default=[],
        help="directory of *.s32x, a single .s32x, or a newline-separated manifest",
    )
    parser.add_argument(
        "--iterations",
        type=int,
        default=1,
        help="number of times to run each target",
    )
    parser.add_argument(
        "--fast-bin",
        default=str(FAST_BIN),
        help="path to slow32-fast executable",
    )
    parser.add_argument(
        "--qemu-bin",
        default=str(QEMU_BIN),
        help="path to qemu-system-slow32 binary",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="print resolved workload list before running",
    )
    args = parser.parse_args()

    ensure_executable(pathlib.Path(args.fast_bin))
    ensure_executable(pathlib.Path(args.qemu_bin))

    images = collect_images(args.image, args.images, args.suite)
    if args.verbose:
        print(f"workloads ({len(images)}): {', '.join(images)}", file=sys.stderr)
    results = []
    for image in images:
        results.append(compare_image(image, args.iterations,
                                     args.fast_bin, args.qemu_bin))

    if len(results) == 1:
        print_single(results[0])
    else:
        print(f"Collected {len(results)} workloads (iterations={args.iterations})\n")
        print_table(results)

    abnormal = [r for r in results if r.abnormal]
    if abnormal:
        names = ", ".join(pathlib.Path(r.image).name for r in abnormal)
        sys.exit(f"error: abnormal exits for: {names} (see warnings above)")


if __name__ == "__main__":
    main()
