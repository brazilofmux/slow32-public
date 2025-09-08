#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#define HEAP_START 0x00100000
#define HEAP_SIZE  0x00100000  // 1MB heap
#define ALIGN_SIZE 8

typedef struct block {
    size_t size;
    struct block *next;
    int free;
} block_t;

static block_t *heap_start = NULL;
static void *heap_end = (void *)(HEAP_START + HEAP_SIZE);

static void init_heap(void) {
    heap_start = (block_t *)HEAP_START;
    heap_start->size = HEAP_SIZE - sizeof(block_t);
    heap_start->next = NULL;
    heap_start->free = 1;
}

static size_t align_size(size_t size) {
    return (size + ALIGN_SIZE - 1) & ~(ALIGN_SIZE - 1);
}

void *malloc(size_t size) {
    if (size == 0) return NULL;
    
    if (!heap_start) init_heap();
    
    size = align_size(size);
    block_t *current = heap_start;
    
    while (current) {
        if (current->free && current->size >= size) {
            if (current->size > size + sizeof(block_t) + ALIGN_SIZE) {
                block_t *new_block = (block_t *)((char *)current + sizeof(block_t) + size);
                new_block->size = current->size - size - sizeof(block_t);
                new_block->next = current->next;
                new_block->free = 1;
                
                current->size = size;
                current->next = new_block;
            }
            
            current->free = 0;
            return (char *)current + sizeof(block_t);
        }
        current = current->next;
    }
    
    return NULL;
}

void free(void *ptr) {
    if (!ptr) return;
    
    block_t *block = (block_t *)((char *)ptr - sizeof(block_t));
    block->free = 1;
    
    block_t *current = heap_start;
    while (current) {
        if (current->free && current->next && current->next->free) {
            current->size += sizeof(block_t) + current->next->size;
            current->next = current->next->next;
        } else {
            current = current->next;
        }
    }
}

void *calloc(size_t nmemb, size_t size) {
    size_t total = nmemb * size;
    if (nmemb != 0 && total / nmemb != size) return NULL;
    
    void *ptr = malloc(total);
    if (ptr) memset(ptr, 0, total);
    return ptr;
}

void *realloc(void *ptr, size_t size) {
    if (!ptr) return malloc(size);
    if (size == 0) {
        free(ptr);
        return NULL;
    }
    
    block_t *block = (block_t *)((char *)ptr - sizeof(block_t));
    if (block->size >= size) return ptr;
    
    void *new_ptr = malloc(size);
    if (new_ptr) {
        memcpy(new_ptr, ptr, block->size);
        free(ptr);
    }
    return new_ptr;
}