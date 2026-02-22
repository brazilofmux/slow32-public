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
#define STAGE5_MAX_CFG_BLOCKS 64
// Allow bounded side-exit lifting so Stage5 can reason about superblock-shaped
// regions. Emission ownership stays gated separately in translate.c.
#define STAGE5_MAX_SIDE_EXITS 2

typedef enum {
    STAGE5_CFG_TERM_FALLTHROUGH = 0,
    STAGE5_CFG_TERM_BRANCH_COND,
    STAGE5_CFG_TERM_JAL_CALL,
    STAGE5_CFG_TERM_JAL_JUMP,
    STAGE5_CFG_TERM_JALR_RET,
    STAGE5_CFG_TERM_JALR_INDIRECT,
    STAGE5_CFG_TERM_HALT,
    STAGE5_CFG_TERM_DEBUG,
    STAGE5_CFG_TERM_YIELD
} stage5_cfg_term_kind_t;

typedef enum {
    STAGE5_CFG_EDGE_NORMAL = 0,
    STAGE5_CFG_EDGE_FALLTHROUGH,
    STAGE5_CFG_EDGE_BRANCH_TAKEN,
    STAGE5_CFG_EDGE_CALL_TARGET,
    STAGE5_CFG_EDGE_CALL_RET_SITE,
    STAGE5_CFG_EDGE_RETURN_CONT
} stage5_cfg_edge_kind_t;

#define STAGE5_CFG_PC_RETURN_CONT 0xFFFFFFFFu

typedef struct {
    uint32_t start_pc;
    uint32_t end_pc;      // exclusive
    uint16_t first_inst;  // index into linear lifted instruction PCs
    uint16_t inst_count;
    stage5_cfg_term_kind_t term_kind;
    uint8_t succ_count;
    uint32_t succ_pc[2];   // successor PCs (may be outside region)
    int16_t succ_block[2]; // successor block index, -1 if outside region/unknown
    stage5_cfg_edge_kind_t succ_kind[2];
    uint32_t def_mask;
    uint32_t use_mask;
    uint32_t live_in_mask;
    uint32_t live_out_mask;
} stage5_cfg_block_t;

typedef struct {
    uint16_t first_def;
    uint16_t last_def;
    uint16_t first_use;
    uint16_t last_use;
    uint16_t def_count;
    uint16_t use_count;
    uint8_t live_blocks;
    bool live_across_edge;
} stage5_reg_flow_t;

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
    uint32_t cfg_block_count;
    bool cfg_valid;
    stage5_cfg_block_t cfg_blocks[STAGE5_MAX_CFG_BLOCKS];
    uint32_t cfg_liveness_iterations;
    uint32_t cfg_max_live;
    bool cfg_spill_likely; // coarse pressure hint: true when max_live > 8
    bool reg_flow_valid;
    stage5_reg_flow_t reg_flow[32];
    uint32_t reg_flow_cross_block_regs;
    uint32_t reg_flow_max_span;
    stage5_lift_reason_t reason;
} stage5_lift_region_t;

void stage5_lift_region_init(stage5_lift_region_t *region, uint32_t start_pc);
bool stage5_lift_superblock(stage5_lift_region_t *region,
                            const uint8_t *mem_base,
                            uint32_t code_limit,
                            uint32_t max_guest_insts);
const char *stage5_lift_reason_str(stage5_lift_reason_t reason);

#endif  // DBT_STAGE5_LIFT_H
