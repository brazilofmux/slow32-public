#ifndef S32_ERRNO_H
#define S32_ERRNO_H

/* The errno a host hands the guest, in the guest's numbering.
 *
 * The guest C libraries (runtime/include/errno.h, selfhost/stage08/
 * include/errno.h) use the Linux numbers, so an MMIO response's errno is a
 * Linux number whatever the host is (docs/SPEC.md 8.4.2).  On Linux that is
 * the host's own errno.  Elsewhere the host's value is looked up here; one
 * the guest has no name for becomes EIO (5).  Before this, a macOS host
 * passed its own numbers through: EAGAIN reached the guest as 35, which the
 * guest calls EDEADLK.
 *
 * Shared by tools/emulator/mmio_ring.c and QEMU's target/slow32/mmio.c,
 * which keeps a copy of this file. */

#include <errno.h>

static inline int s32_errno_from_host(int e)
{
#if defined(__linux__)
    return e;
#else
    static const struct { int host, guest; } map[] = {
        { EPERM, 1 }, { ENOENT, 2 }, { ESRCH, 3 }, { EINTR, 4 }, { EIO, 5 },
        { ENXIO, 6 }, { E2BIG, 7 }, { ENOEXEC, 8 }, { EBADF, 9 }, { ECHILD, 10 },
        { EAGAIN, 11 }, { EWOULDBLOCK, 11 }, { ENOMEM, 12 }, { EACCES, 13 },
        { EFAULT, 14 },
#ifdef ENOTBLK
        { ENOTBLK, 15 },
#endif
        { EBUSY, 16 }, { EEXIST, 17 }, { EXDEV, 18 }, { ENODEV, 19 },
        { ENOTDIR, 20 }, { EISDIR, 21 }, { EINVAL, 22 }, { ENFILE, 23 },
        { EMFILE, 24 }, { ENOTTY, 25 }, { ETXTBSY, 26 }, { EFBIG, 27 },
        { ENOSPC, 28 }, { ESPIPE, 29 }, { EROFS, 30 }, { EMLINK, 31 },
        { EPIPE, 32 }, { EDOM, 33 }, { ERANGE, 34 }, { EDEADLK, 35 },
        { ENAMETOOLONG, 36 }, { ENOSYS, 38 }, { ENOTEMPTY, 39 }, { ELOOP, 40 },
        { EOVERFLOW, 75 }, { EILSEQ, 84 },
        { ENOTSOCK, 88 }, { EDESTADDRREQ, 89 }, { EMSGSIZE, 90 },
        { EPROTOTYPE, 91 }, { ENOPROTOOPT, 92 }, { EPROTONOSUPPORT, 93 },
#ifdef ESOCKTNOSUPPORT
        { ESOCKTNOSUPPORT, 94 },
#endif
        { EOPNOTSUPP, 95 }, { ENOTSUP, 95 }, { EAFNOSUPPORT, 97 },
        { EADDRINUSE, 98 }, { EADDRNOTAVAIL, 99 }, { ENETDOWN, 100 },
        { ENETUNREACH, 101 }, { ENETRESET, 102 }, { ECONNABORTED, 103 },
        { ECONNRESET, 104 }, { ENOBUFS, 105 }, { EISCONN, 106 },
        { ENOTCONN, 107 },
#ifdef ESHUTDOWN
        { ESHUTDOWN, 108 },
#endif
        { ETIMEDOUT, 110 }, { ECONNREFUSED, 111 },
#ifdef EHOSTDOWN
        { EHOSTDOWN, 112 },
#endif
        { EHOSTUNREACH, 113 }, { EALREADY, 114 }, { EINPROGRESS, 115 },
#ifdef ESTALE
        { ESTALE, 116 },
#endif
#ifdef EDQUOT
        { EDQUOT, 122 },
#endif
        { ECANCELED, 125 },
    };
    for (unsigned i = 0; i < sizeof map / sizeof map[0]; i++)
        if (map[i].host == e) return map[i].guest;
    return 5;
#endif
}

#endif
