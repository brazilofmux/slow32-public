/* a64_reloc_kinds.h — small shared header for cg_cpatch_kind[] values.
 *
 * Both codegen_a64.h (which populates the field) and obj_writer.h
 * (which reads it back to map onto R_AARCH64_* relocation types)
 * need these constants. Extracted to break the include cycle.
 */

#ifndef A64_RELOC_KINDS_H
#define A64_RELOC_KINDS_H

#define A64K_CALL26        0   /* BL imm26               -> R_AARCH64_CALL26 */
#define A64K_JUMP26        1   /* B  imm26               -> R_AARCH64_JUMP26 */
#define A64K_ADR_HI21      2   /* ADRP imm21 (page hi)   -> R_AARCH64_ADR_PREL_PG_HI21 */
#define A64K_ADD_LO12      3   /* ADD  imm12 (low12)     -> R_AARCH64_ADD_ABS_LO12_NC */
#define A64K_LDST8_LO12    4   /* LDR/STR byte imm12     -> R_AARCH64_LDST8_ABS_LO12_NC */
#define A64K_LDST16_LO12   5   /* LDR/STR half imm12     -> R_AARCH64_LDST16_ABS_LO12_NC */
#define A64K_LDST32_LO12   6   /* LDR/STR W   imm12      -> R_AARCH64_LDST32_ABS_LO12_NC */
#define A64K_LDST64_LO12   7   /* LDR/STR X   imm12      -> R_AARCH64_LDST64_ABS_LO12_NC */

#endif /* A64_RELOC_KINDS_H */
