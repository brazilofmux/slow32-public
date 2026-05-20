/* dirent.h -- s12cc-compatible stub
 *
 * mmio_ring.h tracks DIR* per host fd; on AArch64 the directory I/O
 * path isn't wired up yet, so DIR is opaque and the helpers are
 * unimplemented (but link OK because nothing in dbt's code path
 * actually opens a directory).
 */
#ifndef _DIRENT_H
#define _DIRENT_H

typedef struct __dir_stream DIR;

struct dirent {
    long d_ino;
    long d_off;
    unsigned short d_reclen;
    unsigned char  d_type;
    char d_name[256];
};

DIR *opendir(char *name);
int  closedir(DIR *dirp);
struct dirent *readdir(DIR *dirp);

#endif
