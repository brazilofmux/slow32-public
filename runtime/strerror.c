/* strerror: what an errno value means.
 *
 * One source for both C libraries (the clang runtime, and the
 * self-hosted library through selfhost/stage08/build-s12cc.sh), so a
 * program says the same thing whichever compiler built it.  The numbers
 * are Linux's, as both <errno.h> have them, and the words are the ones
 * a Linux C library uses for them.
 */
#include <string.h>

struct errtext {
    int code;
    const char *text;
};

static const struct errtext errtexts[] = {
    {0, "Success"},
    {1, "Operation not permitted"},
    {2, "No such file or directory"},
    {3, "No such process"},
    {4, "Interrupted system call"},
    {5, "Input/output error"},
    {6, "No such device or address"},
    {7, "Argument list too long"},
    {8, "Exec format error"},
    {9, "Bad file descriptor"},
    {10, "No child processes"},
    {11, "Resource temporarily unavailable"},
    {12, "Cannot allocate memory"},
    {13, "Permission denied"},
    {14, "Bad address"},
    {16, "Device or resource busy"},
    {17, "File exists"},
    {18, "Invalid cross-device link"},
    {19, "No such device"},
    {20, "Not a directory"},
    {21, "Is a directory"},
    {22, "Invalid argument"},
    {23, "Too many open files in system"},
    {24, "Too many open files"},
    {25, "Inappropriate ioctl for device"},
    {27, "File too large"},
    {28, "No space left on device"},
    {29, "Illegal seek"},
    {30, "Read-only file system"},
    {31, "Too many links"},
    {32, "Broken pipe"},
    {33, "Numerical argument out of domain"},
    {34, "Numerical result out of range"},
    {38, "Function not implemented"},
    {88, "Socket operation on non-socket"},
    {89, "Destination address required"},
    {90, "Message too long"},
    {93, "Protocol not supported"},
    {95, "Operation not supported"},
    {97, "Address family not supported by protocol"},
    {98, "Address already in use"},
    {101, "Network is unreachable"},
    {104, "Connection reset by peer"},
    {105, "No buffer space available"},
    {106, "Transport endpoint is already connected"},
    {107, "Transport endpoint is not connected"},
    {110, "Connection timed out"},
    {111, "Connection refused"},
    {113, "No route to host"},
    {114, "Operation already in progress"},
    {115, "Operation now in progress"},
};

char *strerror(int errnum) {
    static char unknown[32];
    char digits[12];
    unsigned int v;
    int i, n, at;

    for (i = 0; i < (int)(sizeof errtexts / sizeof errtexts[0]); i++) {
        if (errtexts[i].code == errnum) return (char *)errtexts[i].text;
    }
    strcpy(unknown, "Unknown error ");
    at = 14;
    v = (unsigned int)errnum;
    if (errnum < 0) {
        unknown[at++] = '-';
        v = 0u - v;
    }
    n = 0;
    do {
        digits[n++] = (char)('0' + v % 10u);
        v /= 10u;
    } while (v > 0);
    while (n > 0) unknown[at++] = digits[--n];
    unknown[at] = 0;
    return unknown;
}
