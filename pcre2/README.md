# PCRE2 on SLOW-32

Pristine PCRE2 10.49 (`src/` of the release tarball, unmodified; licence in
`LICENCE.md`), built as a library for programs on the target. It is here
because TinyMUX needs it: its softcode regular-expression functions are PCRE2.

    ./build.sh        # out/libpcre2-8.s32a, out/libpcre2-posix.s32a, out/pcre2.h, out/pcre2posix.h, out/pcre2test.s32x
    ./run-tests.sh    # PCRE2's own RunTest over the release's testdata/

8-bit code units with Unicode (UTF-8 and UCP). No JIT: code is execute-only
on SLOW-32, and PCRE2 compiles its no-JIT stubs in `pcre2_jit_compile.c`
(callers that ask for JIT get an error code and fall back to the
interpreter). No 16- or 32-bit libraries.

Link it like libc, before the libc archives, and compile against it with
`-DPCRE2_STATIC -DPCRE2_CODE_UNIT_WIDTH=8 -Ipcre2/out`:

    s32-ld --mmio 64K -o app.s32x runtime/crt0.s32o app.s32o \
        pcre2/out/libpcre2-8.s32a runtime/libc_mmio.s32a runtime/libs32.s32a

## Tests

`run-tests.sh` runs the release's `RunTest -8` with `pcre2test.s32x` on
`slow32-dbt`. RunTest diffs every output against the release's expected
output. The test data is not in this tree: unpack the release and point
`PCRE2_SRC` at it (default `~/refs/pcre2-10.49`).

First run, 2026-10-20: all tests pass with no change to PCRE2.
- Tests 11-13 skip because they are 16/32-bit.
- Test 17 is JIT-only.
- Test 23 is for builds with `\C` disabled.
- Tests 28-29 are EBCDIC-only.
- Test 3, the locale-specific features, skips because none of the
  `fr_FR` locales can be set.

A wrong byte in one match line fails test 1 with a diff, so the
comparison is real.
