// slow32_decode.sv — Instruction decoder + immediate generation
//
// Extracts opcode, register addresses, and immediate values from raw instruction.
// Immediate formats match selfhost/stage00/s32-emu.c exactly.

import slow32_pkg::*;

module slow32_decode (
    input  logic [31:0] instr,

    output logic [6:0]  opcode,
    output logic [4:0]  rd,
    output logic [4:0]  rs1,
    output logic [4:0]  rs2,

    // All immediate formats — consumer picks the right one
    output logic [31:0] imm_i,   // Sign-extended 12-bit
    output logic [31:0] imm_iz,  // Zero-extended 12-bit
    output logic [31:0] imm_s,   // Sign-extended S-type (stores)
    output logic [31:0] imm_b,   // Sign-extended B-type (branches, PC+4 relative)
    output logic [31:0] imm_u,   // U-type (upper 20 bits)
    output logic [31:0] imm_j    // Sign-extended J-type (JAL)
);

    // Field extraction
    assign opcode = instr[6:0];
    assign rd     = instr[11:7];
    assign rs1    = instr[19:15];
    assign rs2    = instr[24:20];

    // I-type: sign-extended 12-bit immediate from bits [31:20]
    wire [11:0] raw_i = instr[31:20];
    assign imm_i = {{20{raw_i[11]}}, raw_i};

    // I-type zero-extended: same bits, no sign extension
    assign imm_iz = {20'd0, raw_i};

    // S-type: imm[4:0] = instr[11:7], imm[11:5] = instr[31:25]
    wire [11:0] raw_s = {instr[31:25], instr[11:7]};
    assign imm_s = {{20{raw_s[11]}}, raw_s};

    // B-type: imm[12|10:5|4:1|11] from instr[31|30:25|11:8|7]
    // Branch offset is 13-bit signed, bit 0 always 0
    wire [12:0] raw_b = {instr[31], instr[7], instr[30:25], instr[11:8], 1'b0};
    assign imm_b = {{19{raw_b[12]}}, raw_b};

    // U-type: upper 20 bits
    assign imm_u = {instr[31:12], 12'd0};

    // J-type: imm[20|10:1|11|19:12] from instr[31|30:21|20|19:12]
    wire [20:0] raw_j = {instr[31], instr[19:12], instr[20], instr[30:21], 1'b0};
    assign imm_j = {{11{raw_j[20]}}, raw_j};

endmodule
