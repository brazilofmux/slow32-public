#!/usr/bin/env python3
"""
Functional parity checks: run .s32x binaries under the reference emulator
and qemu-system-slow32, then compare guest-visible stdout and exit codes.

Example:
    python3 scripts/slow32/functional.py \\
        --suite ~/slow-32/selfhost/stage01 \\
        --suite ~/slow-32/examples
"""

from __future__ import annotations

import argparse
import os
import pathlib
import re
import subprocess
import sys
from dataclasses import dataclass
from typing import List, Optional, Sequence, Tuple


ROOT = pathlib.Path(__file__).resolve().parents[2]
QEMU_BIN = ROOT / "build" / "qemu-system-slow32"


def _default_ref_bin() -> pathlib.Path:
    candidates = [
        pathlib.Path(os.environ["SLOW32_ROOT"]) / "tools" / "emulator" / "slow32"
        if "SLOW32_ROOT" in os.environ
        else None,
        pathlib.Path.home() / "slow-32" / "tools" / "emulator" / "slow32",
        ROOT.parent / "slow-32" / "tools" / "emulator" / "slow32",
        ROOT / "slow-32" / "tools" / "emulator" / "slow32",
    ]
    for path in candidates:
        if path is not None and path.exists():
            return path
    return pathlib.Path.home() / "slow-32" / "tools" / "emulator" / "slow32"


REF_BIN = _default_ref_bin()

# Noise lines from the reference emulator banner / halt footer.
REF_NOISE = re.compile(
    r"^(Starting execution|Program halted\.|HALT at PC=|"
    r"Instructions executed:|Cycles:|Wall time:|Performance:|"
    r"Exit code:)"
)
# Loader chatter and qemu log lines that are not guest output.
# Intrinsic dump is multi-line: a "slow32: native intrinsics" header
# followed by indented "  memcpy:" / "  memset:" rows.
QEMU_NOISE = re.compile(
    r"^(slow32:|Slow32 stats:|ASSERT_EQ failed|"
    r"  (memcpy|memset|memmove|strlen|memswap):)"
)


@dataclass
class CaseResult:
    image: str
    ok: bool
    ref_exit: int
    qemu_exit: int
    detail: str = ""


def run_cmd(argv: Sequence[str], timeout: float
            ) -> Tuple[int, str, str]:
    try:
        proc = subprocess.run(
            argv,
            text=True,
            encoding="utf-8",
            errors="replace",
            capture_output=True,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired as exc:
        out = (exc.stdout or "") if isinstance(exc.stdout, str) else ""
        err = (exc.stderr or "") if isinstance(exc.stderr, str) else ""
        return 124, out, err + f"\n[timeout after {timeout}s]"
    return proc.returncode, proc.stdout, proc.stderr


def ensure_executable(path: pathlib.Path) -> None:
    if not path.exists():
        sys.exit(f"error: {path} does not exist")
    if not os.access(path, os.X_OK):
        sys.exit(f"error: {path} is not executable")


def collect_images(images: Sequence[str], suite_paths: Sequence[str]
                   ) -> List[str]:
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
            if path.suffix == ".s32x":
                add(str(path))
                continue
            for line in path.read_text(encoding="utf-8",
                                       errors="replace").splitlines():
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                add(line)
            continue
        sys.exit(f"error: suite source '{entry}' does not exist")

    return ordered


def guest_stdout(text: str, noise: re.Pattern[str]) -> str:
    lines = []
    for line in text.splitlines():
        if noise.search(line):
            continue
        lines.append(line.rstrip())
    # Drop leading/trailing blank lines for stable diffs.
    while lines and not lines[0]:
        lines.pop(0)
    while lines and not lines[-1]:
        lines.pop()
    return "\n".join(lines)


def compare_one(image: str, ref_bin: str, qemu_bin: str,
                timeout: float) -> CaseResult:
    ref_ec, ref_out, ref_err = run_cmd([ref_bin, image], timeout)
    # Default run: no -serial so console uses the stdout fallback (same
    # path as most CLI smoke tests). Host logs land on stderr.
    qemu_ec, qemu_out, qemu_err = run_cmd(
        [
            qemu_bin,
            "-machine",
            "slow32-tcg",
            "-kernel",
            image,
            "-display",
            "none",
            "-monitor",
            "none",
        ],
        timeout,
    )

    # Reference banner is on stdout; guest usage/errors may be on stderr
    # (e.g. mdfix help text). Combine both streams, then strip noise.
    ref_guest = guest_stdout(ref_out + "\n" + ref_err, REF_NOISE)
    qemu_guest = guest_stdout(qemu_out + "\n" + qemu_err, QEMU_NOISE)

    # Mask process exit to 8 bits (shell / wait status convention).
    ref_st = ref_ec & 0xFF
    qemu_st = qemu_ec & 0xFF

    problems = []
    if ref_st != qemu_st:
        problems.append(f"exit ref={ref_st} qemu={qemu_st}")
    if ref_guest != qemu_guest:
        problems.append("stdout mismatch")
        # Keep the detail short for the summary line.
        problems.append(
            f"  ref : {ref_guest!r}\n  qemu: {qemu_guest!r}"
        )

    if problems:
        return CaseResult(
            image=image,
            ok=False,
            ref_exit=ref_st,
            qemu_exit=qemu_st,
            detail="\n".join(problems),
        )
    return CaseResult(
        image=image, ok=True, ref_exit=ref_st, qemu_exit=qemu_st
    )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "images",
        nargs="*",
        help=".s32x images to compare",
    )
    parser.add_argument(
        "--suite",
        action="append",
        default=[],
        help="directory of *.s32x, a single .s32x, or a manifest file",
    )
    parser.add_argument(
        "--ref-bin",
        default=str(REF_BIN),
        help="path to the reference slow32 emulator",
    )
    parser.add_argument(
        "--qemu-bin",
        default=str(QEMU_BIN),
        help="path to qemu-system-slow32",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="per-binary timeout in seconds (default: 30)",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="print each image path as it runs",
    )
    args = parser.parse_args()

    ensure_executable(pathlib.Path(args.ref_bin))
    ensure_executable(pathlib.Path(args.qemu_bin))

    images = collect_images(args.images, args.suite)
    if not images:
        sys.exit("error: no images provided (pass paths or --suite)")

    passed = failed = 0
    failures: List[CaseResult] = []

    for image in images:
        if args.verbose:
            print(f"run {image}", file=sys.stderr)
        result = compare_one(image, args.ref_bin, args.qemu_bin, args.timeout)
        name = pathlib.Path(result.image).name
        if result.ok:
            print(f"PASS  {name}  (exit {result.ref_exit})")
            passed += 1
        else:
            print(f"FAIL  {name}")
            print(result.detail)
            failed += 1
            failures.append(result)

    print()
    print(f"{passed} passed, {failed} failed, {passed + failed} total")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
