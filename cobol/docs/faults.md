# Fault injection: the I/O error paths

A program's error paths run only when the device misbehaves, so ordinary
tests never reach them. Those paths are the FILE STATUS codes, the USE
AFTER ERROR declaratives and EC-I-O. They are also the paths a batch job
relies on to fail cleanly. The host's MMIO service can now be told to
misbehave:

    S32_FAULT=OP:N:ERR[,OP:N:ERR...]

That fails the Nth request of kind OP with errno ERR, as if the host had
failed it (tools/emulator/mmio_ring.c, ahead of the request's own
handling):

- **OP** is OPEN, CLOSE, READ, WRITE, SEEK, STAT, FLUSH, READ_DIRECT,
  FTRUNCATE, UNLINK, RENAME, ACCESS or LSTAT.
- **ERR** is a number, or ENOENT, EACCES, EPERM, EROFS, EISDIR, ENOSPC,
  EFBIG, EIO, EBADF, EMFILE, EINVAL or EEXIST.
- **N** counts from 1, and 0 fails every one.

Requests on the standard streams (DISPLAY, ACCEPT) are not counted, so
`WRITE:2` is the program's second write to a file. The guest's libc makes
the error `errno` (runtime/mmio_request.c), and the program sees what it
would see for the real failure. Every engine shares the service, so a
test may set it in its `.env` file. GnuCOBOL cannot be given the same
faults, so these tests carry "no oracle", and X3.23-1985 VII-3 decides.

libcob maps the error to the status the text gives it:

| errno | where | status |
|---|---|---|
| ENOENT | OPEN | 35, or 05 for an OPTIONAL file (absent) |
| EACCES, EPERM, EROFS, EISDIR | OPEN | 37: the file will not support the open mode |
| ENOSPC, EFBIG | WRITE | 34: beyond the file's externally defined boundaries |
| any | CLOSE, writing the last buffered records | 30 |
| anything else | | 30 |

## What it found (2026-09-30)

- **An OPTIONAL file that could not be read was taken for absent.**
  OPEN INPUT gave 05, and the program read an empty file. OPEN I-O or
  EXTEND created the file anew, truncating the one that was there: a
  sequential file through its EXTEND probe, an indexed one directly.
  libcob never read errno, so every OPEN failure was 35 or 30. Tests:
  free/faultopen, free/faultidx.
- **A full device at CLOSE reported 00.** libcob ignored fclose's result,
  and the guest libc's fclose ignored its own flush (runtime/stdio.c).
  Test: free/faultwrite.
- **A full device at a WRITE was 30, not 34.** Test: free/faultwrite.

Each test was run against the libcob it replaced, and fails there.
Buffered output reports a device error at the first statement that sees
it, so records buffered before that statement are lost with it. The test
says so rather than hiding it.
