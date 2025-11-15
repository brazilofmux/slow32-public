/*
 * QEMU Slow32 CPU QOM header (target agnostic)
 *
 * Copyright (c) 2011-2012 Jia Liu <proljc@gmail.com>
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 */

#ifndef QEMU_SLOW32_CPU_QOM_H
#define QEMU_SLOW32_CPU_QOM_H

#include "hw/core/cpu.h"

#define TYPE_SLOW32_CPU "slow32-cpu"

OBJECT_DECLARE_CPU_TYPE(Slow32CPU, Slow32CPUClass, SLOW32_CPU)

#define SLOW32_CPU_TYPE_SUFFIX "-" TYPE_SLOW32_CPU
#define SLOW32_CPU_TYPE_NAME(model) model SLOW32_CPU_TYPE_SUFFIX

#endif
