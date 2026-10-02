# SLOW-32 Runtime — Issues & Recommendations

This document tracks bugs, architectural limitations, and opportunities for improvement in the SLOW-32 C runtime and standard library.

## Critical Bugs & Safety Issues

### 1. `printf` / `fprintf` Silent Truncation (Resolved)
Both `printf` and `fprintf` used a local `buffer[1024]` on the stack.

- **Status**: Fixed in `903c892`. `vsnprintf_enhanced` now supports size-querying (two-pass), and the formatting functions dynamically allocate a heap buffer if the output exceeds the stack limit.

### 2. Missing `errno` Wiring (Resolved, refined 2026-08)
The `errno` variable existed but was not set by MMIO-based system calls.

- **Status**: Fixed initially in `903c892` (EINTR / ERR→EIO). Refined 2026-08:
  hosts put a positive errno in the response `length` field on
  `S32_MMIO_STATUS_ERR`; `mmio_request.c` maps it into guest `errno`
  (ENOENT, EBADF, EINVAL, …) with EIO fallback.
- **Open refinement (2026-08 review)**: the emulator passes *raw host* errno
  values, so the guest sees whatever the host OS numbers them. This works
  for the common set because macOS and Linux agree there (ENOENT=2, EBADF=9,
  EACCES=13, EINVAL=22), but higher values diverge (EAGAIN is 35 on macOS,
  11 on Linux), so guest code comparing against those is host-dependent. A
  canonical `S32_ERRNO_*` enum in `mmio_ring_layout.h` — host maps
  host→canonical in `mmio_fail()`, guest maps canonical→its `errno.h` — would
  make the wire format host-independent. Low urgency: no current guest code
  tests errnos outside the agreeing set.

### 3. `pow()` Arbitrary Fast-Path Limit (Resolved)
The `pow(x, y)` implementation had an arbitrary fast-path limit for integer exponents.

- **Status**: Fixed in `43a7926`. Removed the `y < 100.0` limit. Binary exponentiation is now used for any integer exponent, providing significant performance gains for large exponents.

### 4. `math.c` Precision Loss for Large $x$
Trigonometric functions use `fmod(x, TWO_PI)` for range reduction.

- **Problem**: For very large values of $x$, `fmod` loses significant precision because `TWO_PI` is not exactly represented.
- **Recommendation**: Implement a more robust range reduction algorithm (like Payne-Hanek).

---

## Performance & Optimization Opportunities

### 5. Hardware FP Instruction Emission (Resolved)
The SLOW-32 ISA provides native `FSQRT.S`, `FSQRT.D`, `FABS.S`, and `FABS.D` instructions.

- **Status**: `math_hw.c` now defines `sqrt`/`fabs`/`sqrtf`/`fabsf` via `__builtin_*` and is compiled with `-fno-builtin -fno-math-errno`, ensuring `FSQRT.*` and `FABS.*` are emitted.
- **Note**: `math_soft.c` contains the remaining libm implementations and is compiled with `-fno-builtin` to avoid recursive lowering.

### 6. Slow BSS Clearing in `crt0.s` (Resolved)
`crt0.s` cleared the BSS section byte-by-byte.

- **Status**: Fixed in `43a7926`. Replaced manual loop with `jal memset`. This allows fast emulators (`slow32-dbt`, `QEMU`) to intercept the call and use native host `memset`, and slow emulators to use the word-optimized `memset` from `intrinsics.s`.

---

## Architectural Opportunities (MMIO & CORDIC)

### 7. CORDIC Math Optimization (Guest-side) (Resolved)

- **Status**: Fixed in `[COMMIT_HASH]`. CORDIC is now the default implementation for `sin`, `cos`, `atan2`, and `exp`.
- **Benefit**: Since `MUL` takes 32 cycles on SLOW-32, CORDIC (using only 1-cycle shifts and adds) is significantly faster. It also reduced the `math_soft.o` binary size by ~12%.
- **Context**: Inspired by the performance gap between classic 8-bit BASICs and OS/9's BASIC09.

### 8. Security Engine (MMIO)

- **Opportunity**: Add MMIO opcodes for `AES-256`, `SHA-256`, and `Ed25519`.
- **Benefit**: Reimplementing crypto in the guest is slow and highly vulnerable to side-channels. Offloading to host-native routines via MMIO provides security, verification, and speed.
- **Implementation**: Define a standard "Security Engine" opcode range (`0x50-0x5F`).

### 9. High-Resolution `TIMER`
The current `TIMER` uses 1-second resolution `time()`.

- **Opportunity**: Use the `GETTIME` (0x30) opcode to provide a microsecond-resolution timer for benchmarking.

### 10. `READ_DIRECT` (Zero-copy I/O)

- **Opportunity**: Ensure all emulators support this opcode to bypass the `S32_MMIO_DATA_BUFFER` for large reads, improving throughput significantly.

### 11. `rewinddir` was a no-op (Resolved 2026-08)

- **Status**: Fixed. Guest issues `S32_MMIO_OP_REWINDDIR` (0x2B); host
  emulators (`mmio_ring.c`, QEMU `mmio.c`) call POSIX `rewinddir` on the
  open `DIR*`.

### 12. `free` double-free freelist corruption (Resolved 2026-08)

- **Status**: Fixed. `free` rejects out-of-heap pointers, already-free
  blocks, and impossible sizes before coalescing.

### 13. DEBUG-libc `exit` dropped main's return value (Resolved 2026-09-03)

Every emulator reports r1 at `halt` as its process exit status
(`slow32.c`: `int exit_code = cpu.regs[1]; return exit_code;`, the same in
slow32-fast and the DBT's `.exit_status = &cpu->regs[1]`). `__slow32_start`
does pass main's return to `exit(rc)`, but `exit_debug.c` discarded it --
`(void)status;` then `halt()` *as a function call*, so r1 held whatever the
last call happened to return. Every program linked against `libc_debug.s32a`
exited 0, whatever main returned; `libc_mmio.s32a` was unaffected because its
`exit` posts `OP_EXIT` with the status through the ring, and the stage08 libc
only works by accident (its `exit:` is a bare `halt`, executed with main's
return still sitting in r1).

Fix: `exit` moves the status into r1 with inline assembly and halts in the
same statement (`add r1, %0, r0 ; halt`). Verified: a clang-built program
returning 15 yields status 15 under slow32, slow32-fast and slow32-dbt, with
and without `--mmio`; the `libc_mmio` path still yields 15; a program
returning 0 still yields 0.

Found while verifying a stage08 regression test by `echo $?`: every hand
check agreed with itself and disagreed with the stage08 runner (which links
the stage08 libc). The runner was right. The regression suite compares
stdout, so it never noticed either.

### 14. stdio's short entries (2026-10-01)

`fwrite` and `fread` each had one body: a byte written to a buffered file
ran the whole of it -- 91 instructions, most of them the frame and the
tests for cases it was not -- and `fputc` was `fwrite` of one byte. A
COBOL program that writes a file a character at a time (majesty's csv2fw:
4.4 million one-byte records) spent a seventh of its instructions there
(`cobol/docs/performance.md`).

Now each is a short entry in front of the general routine, as `fgetc`
already was. One byte to or from a fully buffered stream whose buffer has
the room, or the byte, is done in the entry, which saves no registers (28
instructions for `fwrite`); a few bytes are a `memcpy` and a count one call
on (`fwrite_more`, `fread_more`); `fputc` stores its byte itself. Anything
else is the general routine's, unchanged: an unbuffered or line-buffered
stream, a memory stream, a buffer about to fill, read-ahead in the buffer,
an element count whose product could overflow. The short paths leave the
buffer short of full, so there is still one place that flushes.

Tests, all in `regression/tests`: `stdio-short-paths` -- random writes,
reads, seeks, `ungetc`, sizes either side of the 4096-byte buffer, against
a model in memory, every count, position and byte checked (the same
program on the host's C library prints the same lines); `stdio-line-order`
-- stdout and stderr on one device, a line out before the stderr line
after it, which is the line-buffered stream's flush the entries must not
skip. Fifteen mutants of the entries and the fixes below, all caught.
Gates: the regression suite, the cross-engine differential, the libc
differential, SQLite's acceptance, Fortran's suite, every COBOL gate, the
dBASE interpreter over majesty's reports (the same bytes), mdfix's 95-run
parity harness.

### 15. Output after input that met end-of-file was lost (Resolved 2026-10-01)

C allows output directly after input when the input met end-of-file, with
no positioning call between. Here the stream's one buffer still held the
reader's bytes (`buf_len` > 0), `fwrite` put the new bytes into it, and
`internal_flush` only writes a buffer it takes for the writer's (`buf_len`
== 0): `fputs` after reading `abc` to the end left `abc`. `fwrite` now
hands the buffer over first, and puts the host's position back over
anything read ahead but not read. Found writing the test for 14;
`regression/tests/stdio-turn`.

### 16. `fseek(f, n, SEEK_CUR)` counted from the read-ahead (Resolved 2026-10-01)

The host's position is the end of what was read into the buffer, and the
seek was passed through relative to that: after one `fgetc` of a 100-byte
file, `fseek(f, 0, SEEK_CUR)` went to byte 100. The offset is now taken
from where the program is -- less the read-ahead, and less one for a
character put back (`ftell` already counted both). `stdio-turn`, and the
random seeks of `stdio-short-paths`.

### 17. `ftell` on a stream opened for append (Resolved 2026-10-01)

`fopen(..., "a")` left the host's position at zero; writes went to the end
all the same, but `ftell` counted from zero and reported the bytes written
since the open as if the file had been empty. An append stream is now
positioned at the end when it is opened (`"a+"`, which reads from the
beginning, is left). `stdio-short-paths` found it once its seeks were in.

### 18. The self-hosted libc's stdio had no buffer at all (Resolved 2026-10-01)

Not this library: `selfhost/stage08/libc/stdio.c`, which the kit's tools
link.  Every `fputc`, `fwrite`, `fgetc` and `fread` there was a `write` or
`read` -- and so was every `fdputc` and `fdgetc`, which is what the tools
actually call.  It is a real stdio now, held to this one by the libc
differential: selfhost ISSUES-73.  (The first note here said the compiler
would gain from it.  It does not: cc buffers its own output and made 517
requests.  The assembler made 2.7 million.)

### 19. `exit` did not write what stdio held (Resolved 2026-10-01)

`exit_mmio.c` went to the host with stdout's last partial line and every
unclosed file's last block still in their buffers: `printf("done")` at the
end of main printed nothing, and a file not fclose'd lost its tail.  (The
COBOL runtime closes its files at STOP RUN for exactly this.)  Streams are
now kept on a list from fopen to fclose; `exit` calls through
`__stdio_exit_hook`, which stdio sets the first time a stream could hold
something -- a pointer, so a program that never touches stdio does not
link it -- and `fflush(NULL)` sends the same streams.
`regression/tests/stdio-exit-flush`, `stdio-exit-stdout`.

### 20. A prompt was not seen before its answer was awaited (Resolved 2026-10-01)

stdout is line buffered, and reading stdin did not send it: after
`printf("name? ")` the program waited for input with the prompt still in
the buffer.  Output waiting in stdout is now sent before stdin is read
(`getchar`, and everything that reads stdin through it).
`regression/tests/stdio-prompt`; `regression/libc-tests/stdio_prompt.c`
holds both libraries to it.

### 21. `strtol`, `strtoul`, `strtoll`, `strtoull`: overflow, and "0x" (Resolved 2026-10-02)

Found when the libc differential got a third leg, the host's C library
(selfhost ISSUES-75): the two libraries here agreed with each other and
were both wrong.

- `strtol` had no overflow handling at all: "2147483648" came back as
  -2147483648 and "99999999999999999999" as 1661992959, `errno` untouched.
- `strtoul`, `strtoll` and `strtoull` stopped at the digit that
  overflowed, so the end pointer was left in the middle of the number,
  and none of them set `ERANGE`.
- All four took "0x" as a prefix whatever followed.  It is one only
  before a hexadecimal digit: in "0x" and "0xg" the number is the 0, and
  the end pointer is at the x.  They converted nothing and left the end
  pointer at the start.
- A base that is none (1, 37) sets `EINVAL`.

`strtol` gathers its value below zero, where a `long` has one more value
than above it, so no test needs a wider type.  The 64-bit pair share one
scan that divides only when the value is within a digit of the top.
`regression/libc-tests/stdlib_misc.c` (against the host too) and
`stdlib_long32.c` (the 32-bit clamps, which are this machine's).

### 22. `mktime` took its argument for UTC and did not normalize; `strftime` (Resolved 2026-10-02)

`mktime` is `localtime`'s inverse.  Here it was `gmtime`'s -- the header
said so -- and it summed the fields as they stood: `tm_mon = 14` indexed
past the month table, `tm_mday = 0` was not the last day of the month
before, and nothing was written back but `tm_wday` and `tm_yday`.  A
program that did `localtime`, changed a field and called `mktime` was
off by the zone's offset.

`strftime` returned the number of bytes it had managed to store when the
result did not fit (the standard: 0), wrote `%c` with a zero-padded day
(the C locale's is `%e`), and had no `%G`, `%g`, `%U`, `%V`, `%W` or
`%r`.

Both are now `time_std.c`, with `asctime`, `ctime` and `difftime`: one
source, built into this library and into the self-hosted one, which had
none of them.  `mktime` finds the instant by asking the host's zone
rules (`__s32_query_tz`) for the offset, twice, and settles the hour a
zone repeats by the caller's `tm_isdst`.  `gmtime_r` and `localtime_r`
are public.  `regression/libc-tests/time_conv.c` holds both builds to
the host's library in a zone with daylight time: 19 instants each way,
ten days across every conversion C99 names, the week-number rules at
six year boundaries, fields out of range in every direction.

### 23. `RAND_MAX` was 2^31-1; `rand` returns 16 bits (Resolved 2026-10-02)

`rand` returns the top 16 bits of a 32-bit state, 0..65535, and
`RAND_MAX` said 0x7FFFFFFF: `rand() / (RAND_MAX + 1.0)` was never above
0.00003, and `rand() > RAND_MAX / 2` was never true.  The sequence is
unchanged -- programs print the numbers they printed -- and `RAND_MAX`
is 0xFFFF.  `stdlib_misc.c` asks that about half of a thousand values
lie above half of `RAND_MAX`.

### 24. `strerror` returned "error" for everything; `perror` printed it (Resolved 2026-10-02)

`strerror.c`: the words a Linux C library uses for the numbers in
`<errno.h>`, "Unknown error N" for the rest.  One source for both
libraries.  `perror` prints `strerror(errno)`.

### 25. `freopen` did not reopen; a closed standard stream kept its buffer (Resolved 2026-10-02)

`freopen` closed the stream and returned whatever `fopen` gave -- a
different `FILE`, except by the accident of `malloc` handing back the
block just freed.  For a standard stream it closed nothing and returned
the new stream: after `freopen("out", "w", stdout)`, `printf` wrote where
it always had.  It is the same `FILE` on the new file now, and a stream
whose new file does not open is closed.

`fclose(stdout)` freed the buffer and left the pointer: a later `printf`
wrote into freed memory.  A closed standard stream has no buffer and no
descriptor.

### 26. `strcasecmp` and `strncasecmp` compared bytes as signed (Resolved 2026-10-02)

"a\x80" sorted before "a\x7f".  They compare as `unsigned char`, as
`strcmp` does.

### 27. New in the library (2026-10-02)

What a hosted C library has and this one did not: `signal` and `raise`
(`signal.c`: a handler runs when the program raises the signal; nothing
else sends one; `signal` in `<signal.h>` was an inline that ignored its
arguments), `abort` through `SIGABRT` -- it was `exit(1)`, which ran
static destructors; it ends the run with 134 and runs nothing --
`atexit` (the list `__cxa_atexit` keeps), `_Exit` and `_exit`, `tmpfile`
(it returned NULL), `getdelim`, `fgetpos`, `fsetpos`, `setbuf`,
`strxfrm`, `system` (there is no command processor: it says so).
`setjmp.s` is written in the operand form both assemblers read, since
the self-hosted library assembles it too; the object is the same bytes.

Still open: `clock` returns 0; `scanf` and `fscanf` are declared and not
defined; the `<ctype.h>` tables are Latin-1, which is not the "C"
locale's answer above 127 (the self-hosted library's functions are
ASCII), and `tolower` applied bytewise to UTF-8 text changes lead bytes.
