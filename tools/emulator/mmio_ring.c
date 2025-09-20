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
    mmio->base_addr = 0;
    
    // Initialize heap
    mmio->brk_current = heap_base;
    mmio->brk_max = heap_base + heap_size;
}

// Map MMIO memory region (returns host pointer to MMIO base)
void* mmio_ring_map(mmio_ring_state_t *mmio) {
    // Allocate 64KB for entire MMIO region
    size_t window_size = S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY;
    void *mmio_mem = mmap(NULL, window_size, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mmio_mem == MAP_FAILED) {
        perror("Failed to map MMIO memory");
        return NULL;
    }
    
    // Set up pointers to ring buffers and data area
    uint8_t *base = (uint8_t*)mmio_mem;
    
    // Skip head/tail registers at offset 0
    mmio->req_ring = (io_descriptor_t*)(base + S32_MMIO_REQ_RING_OFFSET);
    mmio->resp_ring = (io_descriptor_t*)(base + S32_MMIO_RESP_RING_OFFSET);
    mmio->data_buffer = base + S32_MMIO_DATA_BUFFER_OFFSET;
    
    // Clear rings
    memset(mmio->req_ring, 0, S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES);
    memset(mmio->resp_ring, 0, S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES);
    
    return mmio_mem;
}

// MMIO read handler
uint32_t mmio_ring_read(mmio_ring_state_t *mmio, uint32_t addr, int size) {
    if (size != 4) return 0;  // Only 32-bit reads

    if (addr < mmio->base_addr) {
        return 0;
    }

    uint32_t rel = addr - mmio->base_addr;
    if (rel >= S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY) {
        return 0;
    }

    switch (rel) {
        case S32_MMIO_REQ_HEAD_OFFSET:
            return mmio->req_head;
        case S32_MMIO_REQ_TAIL_OFFSET:
            return mmio->req_tail;
        case S32_MMIO_RESP_HEAD_OFFSET:
            return mmio->resp_head;
        case S32_MMIO_RESP_TAIL_OFFSET:
            return mmio->resp_tail;
        default:
            // Reading from rings or data buffer
            if (rel >= S32_MMIO_REQ_RING_OFFSET &&
                rel < S32_MMIO_REQ_RING_OFFSET + S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES) {
                uint32_t offset = (rel - S32_MMIO_REQ_RING_OFFSET) / 4;
                return ((uint32_t*)mmio->req_ring)[offset];
            }
            if (rel >= S32_MMIO_RESP_RING_OFFSET &&
                rel < S32_MMIO_RESP_RING_OFFSET + S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES) {
                uint32_t offset = (rel - S32_MMIO_RESP_RING_OFFSET) / 4;
                return ((uint32_t*)mmio->resp_ring)[offset];
            }
            if (rel >= S32_MMIO_DATA_BUFFER_OFFSET &&
                rel < S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY) {
                uint32_t offset = rel - S32_MMIO_DATA_BUFFER_OFFSET;
                uint32_t value = 0;
                memcpy(&value, mmio->data_buffer + offset, 4);
                return value;
            }
            return 0;
    }
}

// MMIO write handler
void mmio_ring_write(mmio_ring_state_t *mmio, mmio_cpu_iface_t *cpu, uint32_t addr, uint32_t value, int size) {
    (void)cpu;  // Reserved for future use (e.g., trapping on special writes)

    if (size != 4) return;  // Only 32-bit writes

    if (addr < mmio->base_addr) {
        return;
    }

    uint32_t rel = addr - mmio->base_addr;
    if (rel >= S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY) {
        return;
    }

    switch (rel) {
        case S32_MMIO_REQ_HEAD_OFFSET:
            mmio->req_head = value % S32_MMIO_RING_ENTRIES;
            break;
        case S32_MMIO_REQ_TAIL_OFFSET:
            mmio->req_tail = value % S32_MMIO_RING_ENTRIES;
            break;
        case S32_MMIO_RESP_HEAD_OFFSET:
            mmio->resp_head = value % S32_MMIO_RING_ENTRIES;
            break;
        case S32_MMIO_RESP_TAIL_OFFSET:
            mmio->resp_tail = value % S32_MMIO_RING_ENTRIES;
            break;
        default:
            // Writing to rings or data buffer
            if (rel >= S32_MMIO_REQ_RING_OFFSET &&
                rel < S32_MMIO_REQ_RING_OFFSET + S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES) {
                uint32_t offset = (rel - S32_MMIO_REQ_RING_OFFSET) / 4;
                ((uint32_t*)mmio->req_ring)[offset] = value;
            }
            else if (rel >= S32_MMIO_RESP_RING_OFFSET &&
                     rel < S32_MMIO_RESP_RING_OFFSET + S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES) {
                uint32_t offset = (rel - S32_MMIO_RESP_RING_OFFSET) / 4;
                ((uint32_t*)mmio->resp_ring)[offset] = value;
            }
            else if (rel >= S32_MMIO_DATA_BUFFER_OFFSET &&
                     rel < S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY) {
                uint32_t offset = rel - S32_MMIO_DATA_BUFFER_OFFSET;
                memcpy(mmio->data_buffer + offset, &value, 4);
            }
            break;
    }
}

// Process a single request
static void process_request(mmio_ring_state_t *mmio, mmio_cpu_iface_t *cpu, io_descriptor_t *req) {
    io_descriptor_t resp = {0};
    resp.opcode = req->opcode;
    resp.offset = req->offset;
    
    switch (req->opcode) {
        case S32_MMIO_OP_NOP:
            resp.status = 0;
            break;
            
        case S32_MMIO_OP_PUTCHAR: {
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint8_t ch = mmio->data_buffer[offset];
            fputc(ch, stdout);
            fflush(stdout);
            resp.status = 0;
            break;
        }
        
        case S32_MMIO_OP_WRITE: {
            int fd = req->status;  // fd is in status field for requests
            if (fd == 1 || fd == 2) {  // stdout or stderr
                FILE *stream = (fd == 1) ? stdout : stderr;
                uint32_t written = 0;
                for (uint32_t i = 0; i < req->length && i < S32_MMIO_DATA_CAPACITY; i++) {
                    fputc(mmio->data_buffer[(req->offset + i) % S32_MMIO_DATA_CAPACITY], stream);
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
        
        case S32_MMIO_OP_READ: {
            int fd = req->status;  // fd is in status field
            if (fd == 0) {  // stdin
                uint32_t read_count = 0;
                for (uint32_t i = 0; i < req->length && i < S32_MMIO_DATA_CAPACITY; i++) {
                    int ch = fgetc(stdin);
                    if (ch == EOF) break;
                    
                    mmio->data_buffer[(req->offset + i) % S32_MMIO_DATA_CAPACITY] = (uint8_t)ch;
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
        
        case S32_MMIO_OP_GETCHAR: {
            int ch = fgetc(stdin);
            if (ch != EOF) {
                mmio->data_buffer[req->offset % S32_MMIO_DATA_CAPACITY] = (uint8_t)ch;
                resp.length = 1;
                resp.status = 0;
            } else {
                resp.length = 0;
                resp.status = EOF;
            }
            break;
        }
        
        case S32_MMIO_OP_BRK: {
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
        
        case S32_MMIO_OP_EXIT:
            if (cpu && cpu->halted) {
                *(cpu->halted) = true;
            }
            resp.status = req->status;  // Exit code
            break;
            
        case S32_MMIO_OP_FLUSH:
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
void mmio_ring_process(mmio_ring_state_t *mmio, mmio_cpu_iface_t *cpu) {
    while (!ring_empty(mmio->req_head, mmio->req_tail)) {
        io_descriptor_t *req = &mmio->req_ring[mmio->req_tail];
        process_request(mmio, cpu, req);
        mmio->req_tail = ring_next(mmio->req_tail);
        mmio->total_requests++;
    }
}
