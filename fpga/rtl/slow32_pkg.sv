// slow32_pkg.sv — Opcodes, types, constants for SLOW-32 CPU
//
// Matches the ISA defined in selfhost/stage00/s32-emu.c

package slow32_pkg;

  // ---- Opcodes ----

  // R-type arithmetic (0x00-0x0F)
  localparam logic [6:0] OP_ADD   = 7'h00;
  localparam logic [6:0] OP_SUB   = 7'h01;
  localparam logic [6:0] OP_XOR   = 7'h02;
  localparam logic [6:0] OP_OR    = 7'h03;
  localparam logic [6:0] OP_AND   = 7'h04;
  localparam logic [6:0] OP_SLL   = 7'h05;
  localparam logic [6:0] OP_SRL   = 7'h06;
  localparam logic [6:0] OP_SRA   = 7'h07;
  localparam logic [6:0] OP_SLT   = 7'h08;
  localparam logic [6:0] OP_SLTU  = 7'h09;
  localparam logic [6:0] OP_MUL   = 7'h0A;
  localparam logic [6:0] OP_MULH  = 7'h0B;
  localparam logic [6:0] OP_DIV   = 7'h0C;
  localparam logic [6:0] OP_REM   = 7'h0D;
  localparam logic [6:0] OP_SEQ   = 7'h0E;
  localparam logic [6:0] OP_SNE   = 7'h0F;

  // I-type arithmetic + extended R-type (0x10-0x1F)
  localparam logic [6:0] OP_ADDI  = 7'h10;
  localparam logic [6:0] OP_ORI   = 7'h11;
  localparam logic [6:0] OP_ANDI  = 7'h12;
  localparam logic [6:0] OP_SLLI  = 7'h13;
  localparam logic [6:0] OP_SRLI  = 7'h14;
  localparam logic [6:0] OP_SRAI  = 7'h15;
  localparam logic [6:0] OP_SLTI  = 7'h16;
  localparam logic [6:0] OP_SLTIU = 7'h17;
  localparam logic [6:0] OP_SGT   = 7'h18;
  localparam logic [6:0] OP_SGTU  = 7'h19;
  localparam logic [6:0] OP_SLE   = 7'h1A;
  localparam logic [6:0] OP_SLEU  = 7'h1B;
  localparam logic [6:0] OP_SGE   = 7'h1C;
  localparam logic [6:0] OP_SGEU  = 7'h1D;
  localparam logic [6:0] OP_XORI  = 7'h1E;
  localparam logic [6:0] OP_MULHU = 7'h1F;

  // U-type (0x20)
  localparam logic [6:0] OP_LUI   = 7'h20;

  // Load instructions (0x30-0x34)
  localparam logic [6:0] OP_LDB   = 7'h30;
  localparam logic [6:0] OP_LDH   = 7'h31;
  localparam logic [6:0] OP_LDW   = 7'h32;
  localparam logic [6:0] OP_LDBU  = 7'h33;
  localparam logic [6:0] OP_LDHU  = 7'h34;

  // Store instructions (0x38-0x3A)
  localparam logic [6:0] OP_STB   = 7'h38;
  localparam logic [6:0] OP_STH   = 7'h39;
  localparam logic [6:0] OP_STW   = 7'h3A;

  // Assert (0x3F)
  localparam logic [6:0] OP_ASSERT_EQ = 7'h3F;

  // Jump instructions (0x40-0x41)
  localparam logic [6:0] OP_JAL   = 7'h40;
  localparam logic [6:0] OP_JALR  = 7'h41;

  // Branch instructions (0x48-0x4D)
  localparam logic [6:0] OP_BEQ   = 7'h48;
  localparam logic [6:0] OP_BNE   = 7'h49;
  localparam logic [6:0] OP_BLT   = 7'h4A;
  localparam logic [6:0] OP_BGE   = 7'h4B;
  localparam logic [6:0] OP_BLTU  = 7'h4C;
  localparam logic [6:0] OP_BGEU  = 7'h4D;

  // System instructions
  localparam logic [6:0] OP_NOP   = 7'h50;
  localparam logic [6:0] OP_YIELD = 7'h51;
  localparam logic [6:0] OP_DEBUG = 7'h52;
  localparam logic [6:0] OP_HALT  = 7'h7F;

  // ---- FSM states ----
  typedef enum logic [2:0] {
    ST_INIT_SP   = 3'd5,
    ST_INIT_FP   = 3'd6,
    ST_FETCH     = 3'd0,
    ST_DECODE    = 3'd1,
    ST_EXECUTE   = 3'd2,
    ST_MEMORY    = 3'd3,
    ST_WRITEBACK = 3'd4
  } state_t;

  // ---- ALU operation codes (internal) ----
  typedef enum logic [4:0] {
    ALU_ADD   = 5'd0,
    ALU_SUB   = 5'd1,
    ALU_XOR   = 5'd2,
    ALU_OR    = 5'd3,
    ALU_AND   = 5'd4,
    ALU_SLL   = 5'd5,
    ALU_SRL   = 5'd6,
    ALU_SRA   = 5'd7,
    ALU_SLT   = 5'd8,
    ALU_SLTU  = 5'd9,
    ALU_MUL   = 5'd10,
    ALU_MULH  = 5'd11,
    ALU_DIV   = 5'd12,
    ALU_REM   = 5'd13,
    ALU_SEQ   = 5'd14,
    ALU_SNE   = 5'd15,
    ALU_SGT   = 5'd16,
    ALU_SGTU  = 5'd17,
    ALU_SLE   = 5'd18,
    ALU_SLEU  = 5'd19,
    ALU_SGE   = 5'd20,
    ALU_SGEU  = 5'd21,
    ALU_MULHU = 5'd22,
    ALU_PASS_B = 5'd23   // Pass operand B through (for LUI)
  } alu_op_t;

endpackage
