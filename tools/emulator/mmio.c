// SLOW-32 MMIO Device Implementation
#include "mmio.h"
#include "slow32.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <errno.h>

// Initialize MMIO devices
void mmio_init(mmio_state_t *mmio) {
    memset(mmio, 0, sizeof(mmio_state_t));

    // Console starts ready to transmit
    mmio->console_status = CONSOLE_TX_READY;
}

// Handle system calls
static void handle_syscall(mmio_state_t *mmio, cpu_state_t *cpu) {
    mmio->syscall_errno = 0;
    
    switch (mmio->syscall_num) {
        case SYS_EXIT:
            // Exit with code in arg1
            cpu->halted = true;
            mmio->syscall_result = mmio->syscall_args[0];
            break;
            
        case SYS_WRITE: {
            // write(fd, buf, count)
            int fd = mmio->syscall_args[0];
            uint32_t buf = mmio->syscall_args[1];
            uint32_t count = mmio->syscall_args[2];
            
            if (fd == 1 || fd == 2) {  // stdout or stderr
                uint32_t written = 0;
                for (uint32_t i = 0; i < count; i++) {
                    uint8_t ch = 0;
                    if (mm_read(&cpu->mm, buf + i, &ch, 1) == 0) {
                        fputc(ch, fd == 1 ? stdout : stderr);
                        written++;
                    } else {
                        break;  // Memory fault
                    }
                }
                fflush(fd == 1 ? stdout : stderr);
                mmio->syscall_result = written;
            } else {
                mmio->syscall_result = -1;
                mmio->syscall_errno = EBADF;
            }
            break;
        }
        
        case SYS_READ: {
            // read(fd, buf, count)
            int fd = mmio->syscall_args[0];
            uint32_t buf = mmio->syscall_args[1];
            uint32_t count = mmio->syscall_args[2];
            
            if (fd == 0) {  // stdin
                uint32_t read_count = 0;
                for (uint32_t i = 0; i < count; i++) {
                    int ch = fgetc(stdin);
                    if (ch == EOF) break;
                    
                    uint8_t byte = (uint8_t)ch;
                    if (mm_write(&cpu->mm, buf + i, &byte, 1) != 0) {
                        break;  // Memory fault
                    }
                    read_count++;
                    
                    if (ch == '\n') break;  // Line buffered
                }
                mmio->syscall_result = read_count;
            } else {
                mmio->syscall_result = -1;
                mmio->syscall_errno = EBADF;
            }
            break;
        }
        
        default:
            mmio->syscall_result = -1;
            mmio->syscall_errno = ENOSYS;  // Not implemented
            break;
    }
}

// MMIO read handler
uint32_t mmio_read(mmio_state_t *mmio, uint32_t addr, int size) {
    // Only support 32-bit reads for now
    if (size != 4) return 0;
    
    switch (addr) {
        // System call interface
        case SYSCALL_RESULT:
            return mmio->syscall_result;
        case SYSCALL_ERRNO:
            return mmio->syscall_errno;
            
        // Console device
        case CONSOLE_STATUS:
            return mmio->console_status;
        case CONSOLE_DATA:
            if (mmio->console_has_input) {
                uint32_t ch = mmio->console_input_char;
                mmio->console_has_input = false;
                mmio->console_status &= ~CONSOLE_RX_READY;
                return ch;
            }
            return 0;
        case CONSOLE_CONTROL:
            return mmio->console_control;
            
        default:
            return 0;
    }
}

// MMIO write handler
void mmio_write(mmio_state_t *mmio, cpu_state_t *cpu, uint32_t addr, uint32_t value, int size) {
    // Only support 32-bit writes for now
    if (size != 4) return;
    
    switch (addr) {
        // System call interface
        case SYSCALL_NUM:
            mmio->syscall_num = value;
            break;
        case SYSCALL_ARG1:
            mmio->syscall_args[0] = value;
            break;
        case SYSCALL_ARG2:
            mmio->syscall_args[1] = value;
            break;
        case SYSCALL_ARG3:
            mmio->syscall_args[2] = value;
            break;
        case SYSCALL_TRIGGER:
            handle_syscall(mmio, cpu);
            break;
            
        // Console device
        case CONSOLE_DATA:
            // Direct console output
            fputc(value & 0xFF, stdout);
            fflush(stdout);
            break;
        case CONSOLE_CONTROL:
            mmio->console_control = value;
            break;
            
        default:
            break;
    }
}

// Console input handler (called when stdin has data)
void mmio_console_input(mmio_state_t *mmio, char ch) {
    mmio->console_input_char = ch;
    mmio->console_has_input = true;
    mmio->console_status |= CONSOLE_RX_READY;
}