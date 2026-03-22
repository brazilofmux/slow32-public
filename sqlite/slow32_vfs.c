/*
 * slow32_vfs.c - SQLite VFS for SLOW-32
 *
 * Maps SQLite's VFS interface to the SLOW-32 MMIO libc:
 *   open, read, write, lseek, close, ftruncate, stat, access, unlink
 *
 * No mmap, no WAL, no file locking (single-process), no dlopen.
 */

#include "sqlite3.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>
#include <time.h>

/* File handle */
typedef struct slow32_file {
    sqlite3_file base;
    int fd;
    char *path;
} slow32_file;

/* Forward declarations */
static int slow32Close(sqlite3_file *pFile);
static int slow32Read(sqlite3_file *pFile, void *buf, int iAmt, sqlite3_int64 iOfst);
static int slow32Write(sqlite3_file *pFile, const void *buf, int iAmt, sqlite3_int64 iOfst);
static int slow32Truncate(sqlite3_file *pFile, sqlite3_int64 size);
static int slow32Sync(sqlite3_file *pFile, int flags);
static int slow32FileSize(sqlite3_file *pFile, sqlite3_int64 *pSize);
static int slow32Lock(sqlite3_file *pFile, int locktype);
static int slow32Unlock(sqlite3_file *pFile, int locktype);
static int slow32CheckReservedLock(sqlite3_file *pFile, int *pResOut);
static int slow32FileControl(sqlite3_file *pFile, int op, void *pArg);
static int slow32SectorSize(sqlite3_file *pFile);
static int slow32DeviceCharacteristics(sqlite3_file *pFile);

static const sqlite3_io_methods slow32_io = {
    1,                              /* iVersion */
    slow32Close,
    slow32Read,
    slow32Write,
    slow32Truncate,
    slow32Sync,
    slow32FileSize,
    slow32Lock,
    slow32Unlock,
    slow32CheckReservedLock,
    slow32FileControl,
    slow32SectorSize,
    slow32DeviceCharacteristics,
    /* v2+ methods not needed */
    0, 0, 0, 0
};

static int slow32Close(sqlite3_file *pFile) {
    slow32_file *p = (slow32_file *)pFile;
    if (p->fd >= 0) {
        close(p->fd);
        p->fd = -1;
    }
    if (p->path) {
        free(p->path);
        p->path = 0;
    }
    return SQLITE_OK;
}

static int slow32Read(sqlite3_file *pFile, void *buf, int iAmt, sqlite3_int64 iOfst) {
    slow32_file *p = (slow32_file *)pFile;
    lseek(p->fd, (int)iOfst, 0 /* SEEK_SET */);
    int got = read(p->fd, buf, iAmt);
    if (got == iAmt) return SQLITE_OK;
    if (got < 0) return SQLITE_IOERR_READ;
    /* Short read: zero-fill the rest */
    memset((char *)buf + got, 0, iAmt - got);
    return SQLITE_IOERR_SHORT_READ;
}

static int slow32Write(sqlite3_file *pFile, const void *buf, int iAmt, sqlite3_int64 iOfst) {
    slow32_file *p = (slow32_file *)pFile;
    lseek(p->fd, (int)iOfst, 0 /* SEEK_SET */);
    int wrote = write(p->fd, buf, iAmt);
    if (wrote == iAmt) return SQLITE_OK;
    return SQLITE_IOERR_WRITE;
}

static int slow32Truncate(sqlite3_file *pFile, sqlite3_int64 size) {
    slow32_file *p = (slow32_file *)pFile;
    if (ftruncate(p->fd, (int)size) < 0) return SQLITE_IOERR_TRUNCATE;
    return SQLITE_OK;
}

static int slow32Sync(sqlite3_file *pFile, int flags) {
    (void)pFile; (void)flags;
    /* No fsync on SLOW-32 — data goes straight through MMIO */
    return SQLITE_OK;
}

static int slow32FileSize(sqlite3_file *pFile, sqlite3_int64 *pSize) {
    slow32_file *p = (slow32_file *)pFile;
    struct stat st;
    if (fstat(p->fd, &st) < 0) return SQLITE_IOERR_FSTAT;
    *pSize = st.st_size;
    return SQLITE_OK;
}

/* No locking — single process */
static int slow32Lock(sqlite3_file *pFile, int locktype) {
    (void)pFile; (void)locktype;
    return SQLITE_OK;
}
static int slow32Unlock(sqlite3_file *pFile, int locktype) {
    (void)pFile; (void)locktype;
    return SQLITE_OK;
}
static int slow32CheckReservedLock(sqlite3_file *pFile, int *pResOut) {
    (void)pFile;
    *pResOut = 0;
    return SQLITE_OK;
}

static int slow32FileControl(sqlite3_file *pFile, int op, void *pArg) {
    (void)pFile; (void)pArg;
    if (op == SQLITE_FCNTL_LOCKSTATE) return SQLITE_OK;
    return SQLITE_NOTFOUND;
}

static int slow32SectorSize(sqlite3_file *pFile) {
    (void)pFile;
    return 512;
}

static int slow32DeviceCharacteristics(sqlite3_file *pFile) {
    (void)pFile;
    return 0;
}

/* VFS methods */

static int slow32Open(sqlite3_vfs *pVfs, const char *zName, sqlite3_file *pFile,
                      int flags, int *pOutFlags) {
    slow32_file *p = (slow32_file *)pFile;
    (void)pVfs;

    p->fd = -1;
    p->path = 0;
    p->base.pMethods = 0;

    int oflags = 0;
    if (flags & SQLITE_OPEN_READWRITE) oflags = O_RDWR;
    else oflags = O_RDONLY;
    if (flags & SQLITE_OPEN_CREATE) oflags |= O_CREAT;

    if (zName == 0) {
        /* Temp file — use a generated name */
        static int tempcount = 0;
        char tempname[64];
        snprintf(tempname, sizeof(tempname), "/tmp/sqlite_tmp_%d", tempcount++);
        zName = tempname;
        oflags = O_RDWR | O_CREAT | O_TRUNC;
    }

    int fd = open(zName, oflags, 0644);
    if (fd < 0) return SQLITE_CANTOPEN;

    p->fd = fd;
    p->path = malloc(strlen(zName) + 1);
    if (p->path) strcpy(p->path, zName);
    p->base.pMethods = &slow32_io;

    if (pOutFlags) *pOutFlags = flags;
    return SQLITE_OK;
}

static int slow32Delete(sqlite3_vfs *pVfs, const char *zName, int syncDir) {
    (void)pVfs; (void)syncDir;
    if (unlink(zName) < 0) {
        return SQLITE_IOERR_DELETE;
    }
    return SQLITE_OK;
}

static int slow32Access(sqlite3_vfs *pVfs, const char *zName, int flags, int *pResOut) {
    (void)pVfs;
    int mode = F_OK;
    if (flags == SQLITE_ACCESS_READWRITE) mode = W_OK;
    *pResOut = (access(zName, mode) == 0);
    return SQLITE_OK;
}

static int slow32FullPathname(sqlite3_vfs *pVfs, const char *zName,
                              int nOut, char *zOut) {
    (void)pVfs;
    /* SLOW-32 has no real filesystem hierarchy — just use the name as-is */
    if ((int)strlen(zName) >= nOut) return SQLITE_CANTOPEN;
    strcpy(zOut, zName);
    return SQLITE_OK;
}

static int slow32Randomness(sqlite3_vfs *pVfs, int nBuf, char *zBuf) {
    (void)pVfs;
    /* Seed with time-based value */
    unsigned int seed = (unsigned int)time(0);
    for (int i = 0; i < nBuf; i++) {
        seed = seed * 1103515245 + 12345;
        zBuf[i] = (char)(seed >> 16);
    }
    return nBuf;
}

static int slow32Sleep(sqlite3_vfs *pVfs, int microseconds) {
    (void)pVfs;
    usleep(microseconds);
    return microseconds;
}

static int slow32CurrentTime(sqlite3_vfs *pVfs, double *pTime) {
    (void)pVfs;
    time_t t = time(0);
    /* Julian day number for Unix epoch is 2440587.5 */
    *pTime = 2440587.5 + (double)t / 86400.0;
    return SQLITE_OK;
}

static int slow32GetLastError(sqlite3_vfs *pVfs, int nBuf, char *zBuf) {
    (void)pVfs; (void)nBuf; (void)zBuf;
    return 0;
}

/* Register the VFS */
int sqlite3_slow32_vfs_register(void) {
    static sqlite3_vfs vfs = {
        1,                          /* iVersion */
        sizeof(slow32_file),        /* szOsFile */
        256,                        /* mxPathname */
        0,                          /* pNext */
        "slow32",                   /* zName */
        0,                          /* pAppData */
        slow32Open,
        slow32Delete,
        slow32Access,
        slow32FullPathname,
        0,                          /* xDlOpen */
        0,                          /* xDlError */
        0,                          /* xDlSym */
        0,                          /* xDlClose */
        slow32Randomness,
        slow32Sleep,
        slow32CurrentTime,
        slow32GetLastError,
        /* v2+ */
        0, 0, 0, 0
    };
    return sqlite3_vfs_register(&vfs, 1 /* make default */);
}

/* SQLite OS interface — called by sqlite3_initialize() when SQLITE_OS_OTHER */
int sqlite3_os_init(void) {
    return sqlite3_slow32_vfs_register();
}

int sqlite3_os_end(void) {
    return SQLITE_OK;
}
