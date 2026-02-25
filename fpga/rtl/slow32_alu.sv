// slow32_alu.sv — ALU for SLOW-32 CPU
//
// Supports all arithmetic, logic, shift, comparison, and multiply/divide operations.
// MUL/MULH/MULHU use `*` (infers DSP blocks on FPGA).
// DIV/REM use `/` and `%` (behavioral for simulation; needs multi-cycle for synthesis).

import slow32_pkg::*;

module slow32_alu (
    input  alu_op_t     op,
    input  logic [31:0] a,      // Operand A (rs1 value or other)
    input  logic [31:0] b,      // Operand B (rs2 value, immediate, or other)
    output logic [31:0] result
);

    // Signed wires for comparison/arithmetic
    wire signed [31:0] sa = $signed(a);
    wire signed [31:0] sb = $signed(b);

    // 64-bit products for MULH/MULHU
    wire signed [63:0] prod_ss = $signed({sa}) * $signed({sb});
    wire        [63:0] prod_uu = {32'd0, a} * {32'd0, b};

    // Division corner cases (matching s32-emu.c):
    //   div-by-zero → 0xFFFFFFFF
    //   INT_MIN / -1 → INT_MIN
    wire div_by_zero  = (b == 32'd0);
    wire div_overflow = (a == 32'h80000000) && (b == 32'hFFFFFFFF);

    logic [31:0] div_result;
    logic [31:0] rem_result;

    always_comb begin
        if (div_by_zero) begin
            div_result = 32'hFFFFFFFF;
            rem_result = a;
        end else if (div_overflow) begin
            div_result = 32'h80000000;
            rem_result = 32'd0;
        end else begin
            div_result = $unsigned($signed(sa) / $signed(sb));
            rem_result = $unsigned($signed(sa) % $signed(sb));
        end
    end

    always_comb begin
        case (op)
            ALU_ADD:    result = a + b;
            ALU_SUB:    result = a - b;
            ALU_XOR:    result = a ^ b;
            ALU_OR:     result = a | b;
            ALU_AND:    result = a & b;
            ALU_SLL:    result = a << b[4:0];
            ALU_SRL:    result = a >> b[4:0];
            ALU_SRA:    result = $unsigned(sa >>> b[4:0]);
            ALU_SLT:    result = {31'd0, sa < sb};
            ALU_SLTU:   result = {31'd0, a < b};
            ALU_MUL:    result = a * b;
            ALU_MULH:   result = prod_ss[63:32];
            ALU_DIV:    result = div_result;
            ALU_REM:    result = rem_result;
            ALU_SEQ:    result = {31'd0, a == b};
            ALU_SNE:    result = {31'd0, a != b};
            ALU_SGT:    result = {31'd0, sa > sb};
            ALU_SGTU:   result = {31'd0, a > b};
            ALU_SLE:    result = {31'd0, sa <= sb};
            ALU_SLEU:   result = {31'd0, a <= b};
            ALU_SGE:    result = {31'd0, sa >= sb};
            ALU_SGEU:   result = {31'd0, a >= b};
            ALU_MULHU:  result = prod_uu[63:32];
            ALU_PASS_B: result = b;
            default:    result = 32'd0;
        endcase
    end

endmodule
