#ifndef SLOW32_UNISTD_H
#define SLOW32_UNISTD_H

#ifdef __cplusplus
extern "C" {
#endif

unsigned int sleep(unsigned int seconds);
int usleep(unsigned int usec);

#ifdef __cplusplus
}
#endif

#endif
