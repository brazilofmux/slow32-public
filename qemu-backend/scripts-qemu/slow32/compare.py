#!/usr/bin/env python3
"""
Quick-and-dirty harness to compare slow32-fast vs. qemu-system-slow32.

Example:
    python3 scripts/slow32/compare.py --image /tmp/s32_smoke.s32x
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
FAST_BIN = ROOT / "slow-32" / "tools" / "emulator" / "slow32-fast"
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
    qemu_translate: float
    qemu_exec: float
    qemu_tb_count: float
    qemu_insns: float
    fast_wall: Optional[float]
    fast_insns: float

    @property
    def speed_ratio(self) -> Optional[float]:
        if self.fast_wall is None or self.qemu_wall <= 0:
            return None
        return self.fast_wall / self.qemu_wall

    @property
    def translate_fraction(self) -> float:
        if self.qemu_wall <= 0:
            return 0.0
        return min(1.0, max(0.0, self.qemu_translate / self.qemu_wall))


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
            for line in path.read_text().splitlines():
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                add(line)
            continue
        sys.exit(f"error: suite source '{entry}' does not exist")

    if not ordered:
        add(default_image)

    return ordered


def run_qemu(cmd: Sequence[str], iterations: int) -> Tuple[float, float, float, float, float]:
    wall_times = []
    insns = []
    translate = []
    exec_times = []
    tb_counts = []

    for i in range(iterations):
        _, proc = run_cmd(cmd)
        if proc.returncode < 0:
            sys.stderr.write(proc.stdout)
            sys.stderr.write(proc.stderr)
            sys.exit(f"slow32-tcg terminated by signal (iteration {i + 1})")
        match = SLOW32_STATS_RE.search(proc.stdout)
        if not match:
            sys.stderr.write(proc.stdout)
            sys.stderr.write(proc.stderr)
            sys.exit("slow32-tcg did not report stats (missing HALT?)")
        if proc.returncode > 0:
            print(
                f"warning: slow32-tcg exited with status {proc.returncode} "
                f"(iteration {i + 1})",
                file=sys.stderr,
            )
        wall_times.append(float(match.group("wall")) / 1000.0)
        insns.append(int(match.group("insns")))
        translate.append(float(match.group("translate")) / 1000.0)
        exec_times.append(float(match.group("exec")) / 1000.0)
        tb_counts.append(int(match.group("tbs")))

    avg_wall = sum(wall_times) / len(wall_times)
    avg_insn = sum(insns) / len(insns)
    avg_translate = sum(translate) / len(translate)
    avg_exec = sum(exec_times) / len(exec_times)
    avg_tbs = sum(tb_counts) / len(tb_counts)
    return avg_wall, avg_insn, avg_translate, avg_exec, avg_tbs


def run_fast(cmd: Sequence[str], iterations: int) -> Tuple[Optional[float], float]:
    wall_times = []
    insns = []
    for i in range(iterations):
        _, proc = run_cmd(cmd)
        if proc.returncode < 0:
            sys.stderr.write(proc.stdout)
            sys.stderr.write(proc.stderr)
            sys.exit(f"slow32-fast terminated by signal (iteration {i + 1})")
        m_insn = FAST_STATS_RE.search(proc.stdout)
        if not m_insn:
            sys.stderr.write(proc.stdout)
            sys.stderr.write(proc.stderr)
            sys.exit("slow32-fast output did not include instruction count")
        if proc.returncode > 0:
            print(
                f"warning: slow32-fast exited with status {proc.returncode} "
                f"(iteration {i + 1})",
                file=sys.stderr,
            )
        insns.append(int(m_insn.group("insns")))
        m_time = FAST_TIME_RE.search(proc.stdout)
        if m_time:
            wall_times.append(float(m_time.group("secs")))
    avg_time = sum(wall_times) / len(wall_times) if wall_times else None
    avg_insn = sum(insns) / len(insns)
    return avg_time, avg_insn


def compare_image(image: str, iterations: int, fast_bin: str,
                  qemu_bin: str) -> CompareResult:
    fast_cmd = [fast_bin, image]
    qemu_cmd = [
        qemu_bin,
        "-machine",
        "slow32-tcg",
        "-kernel",
        image,
        "-nographic",
        "-monitor",
        "none",
        "-serial",
        "mon:stdio",
    ]

    fast_time, guest_insns = run_fast(fast_cmd, iterations)
    qemu_time, qemu_insns, qemu_translate, qemu_exec, qemu_tbs = run_qemu(
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
        fast_wall=fast_time,
        fast_insns=guest_insns,
    )


def print_single(result: CompareResult) -> None:
    print("slow32 comparison (lower is better)")
    print("-----------------------------------")
    print(f" image: {result.image}")
    print(f" iterations: {result.iterations}")
    print()
    print(
        " qemu-system-slow32: "
        f"{result.qemu_wall:.6f}s (translate {result.qemu_translate:.6f}s, "
        f"exec {result.qemu_exec:.6f}s, tb_count≈{int(result.qemu_tb_count)})"
    )
    if result.fast_wall is not None:
        print(f" slow32-fast:        {result.fast_wall:.6f}s")
    else:
        print(" slow32-fast:        <no wall clock available>")
    print(
        " guest instructions: "
        f"{int(result.fast_insns)} (qemu reported {int(result.qemu_insns)})"
    )
    ratio = result.speed_ratio
    if ratio is not None:
        print(f" speed ratio (fast/tcg): {ratio:.3f}x")
    print(
        f" translate share: {result.translate_fraction * 100:.1f}% "
        "(relative to total qemu time)"
    )


def print_table(results: Sequence[CompareResult]) -> None:
    header = (
        f"{'image':28} {'wall (s)':>10} {'xlate (ms)':>12} "
        f"{'exec (ms)':>11} {'% xlate':>8} {'TBs':>8} {'fast (s)':>10} {'ratio':>8}"
    )
    print(header)
    print("-" * len(header))
    for res in results:
        ratio = res.speed_ratio
        ratio_str = f"{ratio:.3f}" if ratio is not None else "n/a"
        fast_wall = res.fast_wall if res.fast_wall is not None else 0.0
        print(
            f"{pathlib.Path(res.image).name:28} "
            f"{res.qemu_wall:10.6f} {res.qemu_translate * 1000:12.3f} "
            f"{res.qemu_exec * 1000:11.3f} "
            f"{res.translate_fraction * 100:8.2f} "
            f"{int(res.qemu_tb_count):8d} "
            f"{fast_wall:10.6f} {ratio_str:>8}"
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
        help="directory or newline-separated file listing .s32x images to sweep",
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


if __name__ == "__main__":
    main()
