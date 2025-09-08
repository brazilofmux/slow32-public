// SLOW-32 Ring Buffer MMIO Implementation
#include "mmio_ring.h"
#include "slow32.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/mman.h>

// Initialize MMIO ring buffers
void mmio_ring_init(mmio_ring_state_t *mmio, uint32_t heap_base, uint32_t heap_size) {
    memset(mmio, 0, sizeof(mmio_ring_state_t));
    
    // Initialize indices
    mmio->req_head = 0;
    mmio->req_tail = 0;
    mmio->resp_head = 0;
    mmio->resp_tail = 0;
    
    // Initialize heap
    mmio->brk_current = heap_base;
    mmio->brk_max = heap_base + heap_size;
}

// Map MMIO memory region (returns host pointer to MMIO base)
void* mmio_ring_map(mmio_ring_state_t *mmio) {
    // Allocate 64KB for entire MMIO region
    void *mmio_mem = mmap(NULL, 0x10000, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mmio_mem == MAP_FAILED) {
        perror("Failed to map MMIO memory");
        return NULL;
    }
    
    // Set up pointers to ring buffers and data area
    uint8_t *base = (uint8_t*)mmio_mem;
    
    // Skip head/tail registers at offset 0
    mmio->req_ring = (io_descriptor_t*)(base + 0x1000);
    mmio->resp_ring = (io_descriptor_t*)(base + 0x3000);
    mmio->data_buffer = base + 0x4000;
    
    // Clear rings
    memset(mmio->req_ring, 0, RING_SIZE * DESC_SIZE);
    memset(mmio->resp_ring, 0, RING_SIZE * DESC_SIZE);
    
    return mmio_mem;
}

// MMIO read handler
uint32_t mmio_ring_read(mmio_ring_state_t *mmio, uint32_t addr, int size) {
    if (size != 4) return 0;  // Only 32-bit reads
    
    switch (addr) {
        case REQ_HEAD:
            return mmio->req_head;
        case REQ_TAIL:
            return mmio->req_tail;
        case RESP_HEAD:
            return mmio->resp_head;
        case RESP_TAIL:
            return mmio->resp_tail;
        default:
            // Reading from rings or data buffer
            if (addr >= REQ_RING && addr < REQ_RING + RING_SIZE * DESC_SIZE) {
                uint32_t offset = (addr - REQ_RING) / 4;
                return ((uint32_t*)mmio->req_ring)[offset];
            }
            if (addr >= RESP_RING && addr < RESP_RING + RING_SIZE * DESC_SIZE) {
                uint32_t offset = (addr - RESP_RING) / 4;
                return ((uint32_t*)mmio->resp_ring)[offset];
            }
            if (addr >= DATA_BUFFER && addr < DATA_BUFFER + DATA_BUF_SIZE) {
                uint32_t offset = addr - DATA_BUFFER;
                uint32_t value = 0;
                memcpy(&value, mmio->data_buffer + offset, 4);
                return value;
            }
            return 0;
    }
}

// MMIO write handler
void mmio_ring_write(mmio_ring_state_t *mmio, struct cpu_state *cpu, uint32_t addr, uint32_t value, int size) {
    if (size != 4) return;  // Only 32-bit writes
    
    switch (addr) {
        case REQ_HEAD:
            mmio->req_head = value % RING_SIZE;
            break;
        case REQ_TAIL:
            mmio->req_tail = value % RING_SIZE;
            break;
        case RESP_HEAD:
            mmio->resp_head = value % RING_SIZE;
            break;
        case RESP_TAIL:
            mmio->resp_tail = value % RING_SIZE;
            break;
        default:
            // Writing to rings or data buffer
            if (addr >= REQ_RING && addr < REQ_RING + RING_SIZE * DESC_SIZE) {
                uint32_t offset = (addr - REQ_RING) / 4;
                ((uint32_t*)mmio->req_ring)[offset] = value;
            }
            else if (addr >= RESP_RING && addr < RESP_RING + RING_SIZE * DESC_SIZE) {
                uint32_t offset = (addr - RESP_RING) / 4;
                ((uint32_t*)mmio->resp_ring)[offset] = value;
            }
            else if (addr >= DATA_BUFFER && addr < DATA_BUFFER + DATA_BUF_SIZE) {
                uint32_t offset = addr - DATA_BUFFER;
                memcpy(mmio->data_buffer + offset, &value, 4);
            }
            break;
    }
}

// Process a single request
static void process_request(mmio_ring_state_t *mmio, cpu_state_t *cpu, io_descriptor_t *req) {
    io_descriptor_t resp = {0};
    resp.opcode = req->opcode;
    resp.offset = req->offset;
    
    switch (req->opcode) {
        case IO_OP_NOP:
            resp.status = 0;
            break;
            
        case IO_OP_PUTCHAR: {
            uint32_t offset = req->offset % DATA_BUF_SIZE;
            uint8_t ch = mmio->data_buffer[offset];
            fputc(ch, stdout);
            fflush(stdout);
            resp.status = 0;
            break;
        }
        
        case IO_OP_WRITE: {
            int fd = req->status;  // fd is in status field for requests
            if (fd == 1 || fd == 2) {  // stdout or stderr
                FILE *stream = (fd == 1) ? stdout : stderr;
                uint32_t written = 0;
                for (uint32_t i = 0; i < req->length && i < DATA_BUF_SIZE; i++) {
                    fputc(mmio->data_buffer[(req->offset + i) % DATA_BUF_SIZE], stream);
                    written++;
                }
                fflush(stream);
                resp.length = written;
                resp.status = 0;
            } else {
                resp.length = 0;
                resp.status = EBADF;
            }
            break;
        }
        
        case IO_OP_READ: {
            int fd = req->status;  // fd is in status field
            if (fd == 0) {  // stdin
                uint32_t read_count = 0;
                for (uint32_t i = 0; i < req->length && i < DATA_BUF_SIZE; i++) {
                    int ch = fgetc(stdin);
                    if (ch == EOF) break;
                    
                    mmio->data_buffer[(req->offset + i) % DATA_BUF_SIZE] = (uint8_t)ch;
                    read_count++;
                    
                    if (ch == '\n') break;  // Line buffered
                }
                resp.length = read_count;
                resp.status = 0;
            } else {
                resp.length = 0;
                resp.status = EBADF;
            }
            break;
        }
        
        case IO_OP_GETCHAR: {
            int ch = fgetc(stdin);
            if (ch != EOF) {
                mmio->data_buffer[req->offset % DATA_BUF_SIZE] = (uint8_t)ch;
                resp.length = 1;
                resp.status = 0;
            } else {
                resp.length = 0;
                resp.status = EOF;
            }
            break;
        }
        
        case IO_OP_BRK: {
            uint32_t requested = req->status;  // New break value in status
            
            if (requested == 0) {
                // Return current break
                resp.status = mmio->brk_current;
                resp.length = 0;
            } else if (requested >= mmio->brk_current && requested <= mmio->brk_max) {
                // Expand heap
                mmio->brk_current = requested;
                resp.status = requested;
                resp.length = 0;
            } else {
                // Failed to expand
                resp.status = mmio->brk_current;
                resp.length = ENOMEM;
            }
            break;
        }
        
        case IO_OP_EXIT:
            cpu->halted = true;
            resp.status = req->status;  // Exit code
            break;
            
        case IO_OP_FLUSH:
            fflush(stdout);
            fflush(stderr);
            resp.status = 0;
            break;
            
        default:
            resp.status = ENOSYS;  // Not implemented
            break;
    }
    
    // Write response
    if (!ring_full(mmio->resp_head, mmio->resp_tail)) {
        mmio->resp_ring[mmio->resp_head] = resp;
        mmio->resp_head = ring_next(mmio->resp_head);
        mmio->total_responses++;
    }
}

// Process pending requests
void mmio_ring_process(mmio_ring_state_t *mmio, cpu_state_t *cpu) {
    while (!ring_empty(mmio->req_head, mmio->req_tail)) {
        io_descriptor_t *req = &mmio->req_ring[mmio->req_tail];
        process_request(mmio, cpu, req);
        mmio->req_tail = ring_next(mmio->req_tail);
        mmio->total_requests++;
    }
}