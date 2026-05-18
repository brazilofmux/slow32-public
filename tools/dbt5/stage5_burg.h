// SLOW-32 DBT Stage 5: BURG selection over lifted regions (scaffold)

#ifndef DBT_STAGE5_BURG_H
#define DBT_STAGE5_BURG_H

#include <stdbool.h>
#include <stdint.h>

#include "stage5_lift.h"
#include "stage5_mir.h"
#include "stage5_lir.h"
#include "stage5_ssa.h"

typedef enum {
    STAGE5_BURG_OK = 0,
    STAGE5_BURG_NOT_IMPLEMENTED,
    STAGE5_BURG_NO_COVER,
    STAGE5_BURG_ILLEGAL_COVER,
    STAGE5_BURG_INTERNAL_ERROR
} stage5_burg_reason_t;

typedef enum {
    STAGE5_BURG_PATTERN_NONE = 0,
    STAGE5_BURG_PATTERN_JAL_CALL_SHORT,
    STAGE5_BURG_PATTERN_JAL_CALL_LONG,
    STAGE5_BURG_PATTERN_JAL_JUMP,
    STAGE5_BURG_PATTERN_JALR_RET_SHORT,
    STAGE5_BURG_PATTERN_JALR_RET_LONG,
    STAGE5_BURG_PATTERN_JALR_INDIRECT,
    STAGE5_BURG_PATTERN_HALT,
    STAGE5_BURG_PATTERN_YIELD,
    STAGE5_BURG_PATTERN_DEBUG,
    STAGE5_BURG_PATTERN_DIRECT_BRANCH_EQ,
    STAGE5_BURG_PATTERN_DIRECT_BRANCH_NE,
    STAGE5_BURG_PATTERN_DIRECT_BRANCH_REL,
    STAGE5_BURG_PATTERN_DIRECT_BRANCH_RELU,
    STAGE5_BURG_PATTERN_CMP_BRANCH_ZERO,
    STAGE5_BURG_PATTERN_CMP_BRANCH_CONST01,
    STAGE5_BURG_PATTERN_CMP_BRANCH_XOR1,
    STAGE5_BURG_PATTERN_CMP_BRANCH_BOOLPAIR,
    STAGE5_BURG_PATTERN_CMP_BRANCH_CMPDEP,
    STAGE5_BURG_PATTERN_CMP_BRANCH_NOCMPDEP,
    STAGE5_BURG_PATTERN_BLOCK_END,
    STAGE5_BURG_PATTERN_GENERIC,
    STAGE5_BURG_PATTERN_COUNT
} stage5_burg_pattern_t;

typedef struct {
    bool selected;
    uint32_t pattern_count;
    uint32_t estimated_cost;
    stage5_burg_reason_t reason;
    stage5_burg_pattern_t pattern;
} stage5_burg_result_t;

void stage5_burg_result_init(stage5_burg_result_t *result);
bool stage5_burg_select(const stage5_lift_region_t *region, stage5_burg_result_t *result);
bool stage5_burg_lower(const stage5_mir_t *mir, const stage5_ssa_overlay_t *ssa, stage5_lir_t *lir);
const char *stage5_burg_reason_str(stage5_burg_reason_t reason);
const char *stage5_burg_pattern_str(stage5_burg_pattern_t pattern);

#endif  // DBT_STAGE5_BURG_H
