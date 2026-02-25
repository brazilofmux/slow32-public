// slow32_alu.sv — ALU for SLOW-32 CPU
//
// Supports all arithmetic, logic, shift, comparison, and multiply operations.
// MUL/MULH/MULHU use `*` (infers DSP blocks on FPGA).
// DIV/REM are handled by the multi-cycle slow32_divider module in the CPU.

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
            ALU_DIV:    result = 32'd0;  // Handled by CPU divider FSM
            ALU_REM:    result = 32'd0;  // Handled by CPU divider FSM
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
