# cross-include: Linux host headers for the cross-built tools

`stage08/include` is the self-hosted libc's own header set, written for
SLOW-32 guests: its `fcntl.h` carries the MMIO ring's open flags, its
`struct stat` is the MMIO stat reply.  The cross compilers (cc-x64,
cc-a64) build host programs -- dbt-x64, dbt-a64 -- that call the Linux
kernel through libc_x64 / libc_a64, and for those the kernel's numbers
and layouts are the right ones.  These headers come first on their
include path (`-I ../cross-include`); everything not here still comes
from `stage08/include`.

Layouts are the kernel's (asm-generic, which AArch64 uses outright and
x86-64 shares for everything here except `struct stat` and two `O_`
flags), as glibc presents them where glibc and the kernel agree.  The
functions are implemented in `libc_linux.c` beside these headers, once
for both hosts; `__x86_64__` / `__aarch64__` choose syscall numbers.
