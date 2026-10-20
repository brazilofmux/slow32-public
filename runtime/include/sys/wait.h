#ifndef SLOW32_SYS_WAIT_H
#define SLOW32_SYS_WAIT_H

/* A program here makes no child processes (fork fails with ENOSYS), so
 * wait and waitpid have none to report: they fail with ECHILD.  The
 * status macros decode Linux's encoding.  process.c. */

#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

#define WNOHANG   1
#define WUNTRACED 2

#define WEXITSTATUS(s)  (((s) & 0xff00) >> 8)
#define WTERMSIG(s)     ((s) & 0x7f)
#define WSTOPSIG(s)     WEXITSTATUS(s)
#define WIFEXITED(s)    (WTERMSIG(s) == 0)
#define WIFSIGNALED(s)  (((signed char)(((s) & 0x7f) + 1) >> 1) > 0)
#define WIFSTOPPED(s)   (((s) & 0xff) == 0x7f)

pid_t wait(int *status);
pid_t waitpid(pid_t pid, int *status, int options);

#ifdef __cplusplus
}
#endif

#endif
