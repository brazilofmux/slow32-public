/* The machine runs one program: it is process 1, with no parent
 * (process.c).  An object of its own, so signal.c's use of it pulls in
 * nothing else. */
#include <unistd.h>

pid_t getpid(void) { return 1; }
pid_t getppid(void) { return 0; }
