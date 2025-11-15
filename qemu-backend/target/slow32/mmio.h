#ifndef SLOW32_MMIO_H
#define SLOW32_MMIO_H

#include "cpu-qom.h"

typedef struct Slow32MMIOContext Slow32MMIOContext;

void slow32_mmio_context_init(Slow32CPU *cpu);
void slow32_mmio_context_destroy(Slow32CPU *cpu);
void slow32_mmio_reset(Slow32CPU *cpu);
void slow32_mmio_reconfigure(Slow32CPU *cpu);
void slow32_mmio_process(Slow32CPU *cpu);

#define S32_MMIO_WINDOW_SIZE 0x00010000u

#endif /* SLOW32_MMIO_H */
