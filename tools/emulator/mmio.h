// SLOW-32 MMIO Device Framework
#ifndef MMIO_H
#define MMIO_H

#include <stdint.h>
#include <stdbool.h>

// MMIO memory map
#define MMIO_BASE        0x10000000
#define MMIO_SIZE        0x00010000  // 64KB total MMIO space

// Device regions
#define SYSCALL_BASE     (MMIO_BASE + 0x0000)
#define CONSOLE_BASE     (MMIO_BASE + 0x1000)
#define TIMER_BASE       (MMIO_BASE + 0x2000)
#define BLOCK_BASE       (MMIO_BASE + 0x3000)

// System call interface registers
#define SYSCALL_NUM      (SYSCALL_BASE + 0x00)
#define SYSCALL_ARG1     (SYSCALL_BASE + 0x04)
#define SYSCALL_ARG2     (SYSCALL_BASE + 0x08)
#define SYSCALL_ARG3     (SYSCALL_BASE + 0x0C)
#define SYSCALL_RESULT   (SYSCALL_BASE + 0x10)
#define SYSCALL_ERRNO    (SYSCALL_BASE + 0x14)
#define SYSCALL_TRIGGER  (SYSCALL_BASE + 0x18)

// Console device registers
#define CONSOLE_STATUS   (CONSOLE_BASE + 0x00)
#define CONSOLE_DATA     (CONSOLE_BASE + 0x04)
#define CONSOLE_CONTROL  (CONSOLE_BASE + 0x08)

// Console status bits
#define CONSOLE_TX_READY  0x01
#define CONSOLE_RX_READY  0x02

// System call numbers
#define SYS_EXIT   0
#define SYS_OPEN   1
#define SYS_CLOSE  2
#define SYS_READ   3
#define SYS_WRITE  4
#define SYS_LSEEK  5
#define SYS_IOCTL  7
#define SYS_FSTAT  8

// Forward declaration
struct cpu_state;

// MMIO device state
typedef struct {
    // System call interface
    uint32_t syscall_num;
    uint32_t syscall_args[3];
    uint32_t syscall_result;
    uint32_t syscall_errno;
    
    // Console device
    uint32_t console_status;
    uint32_t console_control;
    bool console_has_input;
    uint8_t console_input_char;
    
} mmio_state_t;

// Initialize MMIO devices
void mmio_init(mmio_state_t *mmio);

// MMIO read/write handlers
uint32_t mmio_read(mmio_state_t *mmio, uint32_t addr, int size);
void mmio_write(mmio_state_t *mmio, struct cpu_state *cpu, uint32_t addr, uint32_t value, int size);

// Console input (called from emulator when stdin has data)
void mmio_console_input(mmio_state_t *mmio, char ch);

#endif // MMIO_H