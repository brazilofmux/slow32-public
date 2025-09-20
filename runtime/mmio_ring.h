#ifndef SLOW32_RUNTIME_MMIO_RING_H
#define SLOW32_RUNTIME_MMIO_RING_H

#include <stdint.h>

#include "../common/mmio_ring_layout.h"

#ifdef __cplusplus
extern "C" {
#endif

// Symbols provided by the linker when --mmio is enabled
extern uint8_t __mmio_base;
extern uint8_t __mmio_end;

static inline uintptr_t s32_mmio_base_addr(void) {
    return (uintptr_t)&__mmio_base;
}

static inline uintptr_t s32_mmio_end_addr(void) {
    return (uintptr_t)&__mmio_end;
}

static inline volatile uint32_t *s32_mmio_reg32_ptr(uint32_t offset) {
    return (volatile uint32_t *)(s32_mmio_base_addr() + (uintptr_t)offset);
}

static inline volatile uint8_t *s32_mmio_reg8_ptr(uint32_t offset) {
    return (volatile uint8_t *)(s32_mmio_base_addr() + (uintptr_t)offset);
}

#define S32_MMIO_REQ_HEAD (*s32_mmio_reg32_ptr(S32_MMIO_REQ_HEAD_OFFSET))
#define S32_MMIO_REQ_TAIL (*s32_mmio_reg32_ptr(S32_MMIO_REQ_TAIL_OFFSET))
#define S32_MMIO_RESP_HEAD (*s32_mmio_reg32_ptr(S32_MMIO_RESP_HEAD_OFFSET))
#define S32_MMIO_RESP_TAIL (*s32_mmio_reg32_ptr(S32_MMIO_RESP_TAIL_OFFSET))
#define S32_MMIO_REQ_RING (s32_mmio_reg32_ptr(S32_MMIO_REQ_RING_OFFSET))
#define S32_MMIO_RESP_RING (s32_mmio_reg32_ptr(S32_MMIO_RESP_RING_OFFSET))
#define S32_MMIO_DATA_BUFFER (s32_mmio_reg8_ptr(S32_MMIO_DATA_BUFFER_OFFSET))

#ifdef __cplusplus
}
#endif

#endif