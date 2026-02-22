// SLOW-32 DBT Stage 5: Superblock lifting IR (scaffold)

#ifndef DBT_STAGE5_LIFT_H
#define DBT_STAGE5_LIFT_H

#include <stdbool.h>
#include <stdint.h>

typedef enum {
    STAGE5_LIFT_OK = 0,
    STAGE5_LIFT_NOT_IMPLEMENTED,
    STAGE5_LIFT_UNSUPPORTED_OPCODE,
    STAGE5_LIFT_REGION_TOO_LARGE,
    STAGE5_LIFT_INVALID_CONTROL_FLOW,
    STAGE5_LIFT_INTERNAL_ERROR
} stage5_lift_reason_t;

typedef enum {
    STAGE5_IR_INVALID = 0,
    STAGE5_IR_CONST,
    STAGE5_IR_GPR_READ,
    STAGE5_IR_GPR_WRITE,
    STAGE5_IR_ADD,
    STAGE5_IR_SUB,
    STAGE5_IR_AND,
    STAGE5_IR_OR,
    STAGE5_IR_XOR,
    STAGE5_IR_SLL,
    STAGE5_IR_SRL,
    STAGE5_IR_SRA,
    STAGE5_IR_CMP,
    STAGE5_IR_BRANCH,
    STAGE5_IR_LOAD,
    STAGE5_IR_STORE
} stage5_ir_node_kind_t;

typedef struct {
    stage5_ir_node_kind_t kind;
    uint8_t opcode;
    uint8_t rd;
    uint8_t rs1;
    uint8_t rs2;
    int32_t imm;
    uint32_t pc;
    bool synthetic;
    bool is_side_exit;   // forward branch converted to superblock side exit
} stage5_ir_node_t;

#define STAGE5_MAX_IR_NODES 128
// Keep side-exit lifting disabled until Stage5 side-exit emission is fully
// hardened against superblock formation pathologies.
#define STAGE5_MAX_SIDE_EXITS 0

typedef struct {
    uint32_t start_pc;
    uint32_t end_pc;
    uint32_t node_count;
    uint32_t value_count;
    uint32_t guest_inst_count;
    bool has_terminal_branch;
    bool ended_by_budget;
    bool has_unsigned_cmp;
    bool has_unsupported_opcode;
    uint8_t unsupported_opcode;
    uint32_t side_exit_count;
    uint32_t ir_count;
    stage5_ir_node_t ir[STAGE5_MAX_IR_NODES];
    stage5_lift_reason_t reason;
} stage5_lift_region_t;

void stage5_lift_region_init(stage5_lift_region_t *region, uint32_t start_pc);
bool stage5_lift_superblock(stage5_lift_region_t *region,
                            const uint8_t *mem_base,
                            uint32_t code_limit,
                            uint32_t max_guest_insts);
const char *stage5_lift_reason_str(stage5_lift_reason_t reason);

#endif  // DBT_STAGE5_LIFT_H
