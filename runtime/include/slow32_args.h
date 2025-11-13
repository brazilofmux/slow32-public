#ifndef SLOW32_ARGS_H
#define SLOW32_ARGS_H

#ifdef __cplusplus
extern "C" {
#endif

int __slow32_fetch_args(int *argc_out, char ***argv_out);
void __slow32_release_args(void);

#ifdef __cplusplus
}
#endif

#endif
