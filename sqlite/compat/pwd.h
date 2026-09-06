/* the shell's ~ expansion: no users on the guest */
#ifndef SLOW32_PWD_H
#define SLOW32_PWD_H
struct passwd { char *pw_name, *pw_dir; };
static inline struct passwd *getpwuid(int uid) { (void)uid; return 0; }
static inline struct passwd *getpwnam(const char *n) { (void)n; return 0; }
#endif
