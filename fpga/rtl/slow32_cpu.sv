// slow32_cpu.sv — Top-level SLOW-32 CPU: multi-cycle FSM + datapath
//
// Architecture: INIT_SP → INIT_FP → FETCH → DECODE → EXECUTE → MEMORY → WRITEBACK
// Memory interface: Harvard-ish (separate instruction/data ports)
// Matches ISA from selfhost/stage00/s32-emu.c exactly.

import slow32_pkg::*;

module slow32_cpu (
    input  logic        clk,
    input  logic        rst_n,

    // Initialization inputs (latched on reset release)
    input  logic [31:0] init_pc,   // Entry point
    input  logic [31:0] init_sp,   // Initial stack pointer (r29 and r30)

    // Instruction memory port
    output logic [31:0] imem_addr,
    input  logic [31:0] imem_rdata,

    // Data memory port
    output logic [31:0] dmem_addr,
    output logic [31:0] dmem_wdata,
    output logic [3:0]  dmem_byte_en,
    output logic        dmem_wen,
    output logic        dmem_ren,
    input  logic [31:0] dmem_rdata,

    // Status outputs
    output logic        halted,
    output logic        debug_valid,
    output logic [7:0]  debug_char,

    // Assert failure output
    output logic        assert_fail,
    output logic [4:0]  assert_rs1,
    output logic [4:0]  assert_rs2,
    output logic [31:0] assert_val1,
    output logic [31:0] assert_val2,
    output logic [31:0] assert_pc
);

    // ---- State machine ----
    state_t state;

    // ---- PC ----
    logic [31:0] pc;

    // ---- Instruction register ----
    logic [31:0] ir;

    // ---- Decoded fields (from decoder, active when ir is valid) ----
    logic [6:0]  dec_opcode;
    logic [4:0]  dec_rd, dec_rs1, dec_rs2;
    logic [31:0] dec_imm_i, dec_imm_iz, dec_imm_s, dec_imm_b, dec_imm_u, dec_imm_j;

    // ---- Register file ----
    logic [31:0] rs1_val, rs2_val;   // Combinational read outputs
    logic [31:0] rs1_reg, rs2_reg;   // Latched copies for EXECUTE/MEMORY
    logic        rf_wr_en;
    logic [4:0]  rf_wr_addr;
    logic [31:0] rf_wr_data;

    // ---- ALU ----
    alu_op_t     alu_op;
    logic [31:0] alu_a, alu_b, alu_result;

    // ---- Memory stage data ----
    logic [31:0] alu_out_reg;        // Latched ALU result
    logic [31:0] mem_load_data;      // Processed load data (sign/zero extended)

    // ---- Control signals ----
    logic is_load, is_store, is_branch, is_jal, is_jalr;
    logic branch_taken;

    // ========================================================================
    // Submodule instantiation
    // ========================================================================

    // Decoder — combinational, driven by IR
    slow32_decode u_decode (
        .instr   (ir),
        .opcode  (dec_opcode),
        .rd      (dec_rd),
        .rs1     (dec_rs1),
        .rs2     (dec_rs2),
        .imm_i   (dec_imm_i),
        .imm_iz  (dec_imm_iz),
        .imm_s   (dec_imm_s),
        .imm_b   (dec_imm_b),
        .imm_u   (dec_imm_u),
        .imm_j   (dec_imm_j)
    );

    // Register file
    slow32_regfile u_regfile (
        .clk      (clk),
        .rst_n    (rst_n),
        .rs1_addr (dec_rs1),
        .rs1_data (rs1_val),
        .rs2_addr (dec_rs2),
        .rs2_data (rs2_val),
        .wr_en    (rf_wr_en),
        .wr_addr  (rf_wr_addr),
        .wr_data  (rf_wr_data)
    );

    // ALU
    slow32_alu u_alu (
        .op     (alu_op),
        .a      (alu_a),
        .b      (alu_b),
        .result (alu_result)
    );

    // ========================================================================
    // Instruction classification
    // ========================================================================
    always_comb begin
        is_load   = (dec_opcode >= OP_LDB && dec_opcode <= OP_LDHU);
        is_store  = (dec_opcode >= OP_STB && dec_opcode <= OP_STW);
        is_jal    = (dec_opcode == OP_JAL);
        is_jalr   = (dec_opcode == OP_JALR);
        is_branch = (dec_opcode >= OP_BEQ && dec_opcode <= OP_BGEU);
    end

    // ========================================================================
    // ALU operation selection
    // ========================================================================
    always_comb begin
        alu_op = ALU_ADD;  // Default
        case (dec_opcode)
            OP_ADD:   alu_op = ALU_ADD;
            OP_SUB:   alu_op = ALU_SUB;
            OP_XOR:   alu_op = ALU_XOR;
            OP_OR:    alu_op = ALU_OR;
            OP_AND:   alu_op = ALU_AND;
            OP_SLL:   alu_op = ALU_SLL;
            OP_SRL:   alu_op = ALU_SRL;
            OP_SRA:   alu_op = ALU_SRA;
            OP_SLT:   alu_op = ALU_SLT;
            OP_SLTU:  alu_op = ALU_SLTU;
            OP_MUL:   alu_op = ALU_MUL;
            OP_MULH:  alu_op = ALU_MULH;
            OP_DIV:   alu_op = ALU_DIV;
            OP_REM:   alu_op = ALU_REM;
            OP_SEQ:   alu_op = ALU_SEQ;
            OP_SNE:   alu_op = ALU_SNE;
            OP_ADDI:  alu_op = ALU_ADD;
            OP_ORI:   alu_op = ALU_OR;
            OP_ANDI:  alu_op = ALU_AND;
            OP_SLLI:  alu_op = ALU_SLL;
            OP_SRLI:  alu_op = ALU_SRL;
            OP_SRAI:  alu_op = ALU_SRA;
            OP_SLTI:  alu_op = ALU_SLT;
            OP_SLTIU: alu_op = ALU_SLTU;
            OP_XORI:  alu_op = ALU_XOR;
            OP_SGT:   alu_op = ALU_SGT;
            OP_SGTU:  alu_op = ALU_SGTU;
            OP_SLE:   alu_op = ALU_SLE;
            OP_SLEU:  alu_op = ALU_SLEU;
            OP_SGE:   alu_op = ALU_SGE;
            OP_SGEU:  alu_op = ALU_SGEU;
            OP_MULHU: alu_op = ALU_MULHU;
            OP_LUI:   alu_op = ALU_PASS_B;
            OP_LDB, OP_LDH, OP_LDW, OP_LDBU, OP_LDHU: alu_op = ALU_ADD;
            OP_STB, OP_STH, OP_STW:                     alu_op = ALU_ADD;
            OP_JAL, OP_JALR: alu_op = ALU_ADD;
            default: alu_op = ALU_ADD;
        endcase
    end

    // ========================================================================
    // ALU operand selection
    // ========================================================================
    always_comb begin
        alu_a = rs1_reg;
        alu_b = rs2_reg;

        case (dec_opcode)
            // I-type with sign-extended immediate
            OP_ADDI, OP_SLLI, OP_SRLI, OP_SRAI, OP_SLTI:
                alu_b = dec_imm_i;

            // I-type with zero-extended immediate
            OP_ORI, OP_ANDI, OP_XORI, OP_SLTIU:
                alu_b = dec_imm_iz;

            // LUI: pass U-type immediate
            OP_LUI: begin
                alu_a = 32'd0;
                alu_b = dec_imm_u;
            end

            // Loads: rs1 + sign-extended I-type offset
            OP_LDB, OP_LDH, OP_LDW, OP_LDBU, OP_LDHU:
                alu_b = dec_imm_i;

            // Stores: rs1 + sign-extended S-type offset
            OP_STB, OP_STH, OP_STW:
                alu_b = dec_imm_s;

            // JALR: rs1 + imm_i (computes target address)
            OP_JALR:
                alu_b = dec_imm_i;

            default: begin
                alu_a = rs1_reg;
                alu_b = rs2_reg;
            end
        endcase
    end

    // ========================================================================
    // Branch condition evaluation
    // ========================================================================
    always_comb begin
        branch_taken = 1'b0;
        case (dec_opcode)
            OP_BEQ:  branch_taken = (rs1_reg == rs2_reg);
            OP_BNE:  branch_taken = (rs1_reg != rs2_reg);
            OP_BLT:  branch_taken = ($signed(rs1_reg) <  $signed(rs2_reg));
            OP_BGE:  branch_taken = ($signed(rs1_reg) >= $signed(rs2_reg));
            OP_BLTU: branch_taken = (rs1_reg <  rs2_reg);
            OP_BGEU: branch_taken = (rs1_reg >= rs2_reg);
            default: branch_taken = 1'b0;
        endcase
    end

    // ========================================================================
    // Memory interface: data port
    // ========================================================================
    assign dmem_addr = alu_out_reg;

    // Store data: rs2_reg positioned on correct byte lanes
    always_comb begin
        dmem_wdata   = 32'd0;
        dmem_byte_en = 4'b0000;
        case (dec_opcode)
            OP_STB: begin
                dmem_wdata   = {4{rs2_reg[7:0]}};
                case (alu_out_reg[1:0])
                    2'd0: dmem_byte_en = 4'b0001;
                    2'd1: dmem_byte_en = 4'b0010;
                    2'd2: dmem_byte_en = 4'b0100;
                    2'd3: dmem_byte_en = 4'b1000;
                endcase
            end
            OP_STH: begin
                dmem_wdata   = {2{rs2_reg[15:0]}};
                case (alu_out_reg[1])
                    1'd0: dmem_byte_en = 4'b0011;
                    1'd1: dmem_byte_en = 4'b1100;
                endcase
            end
            OP_STW: begin
                dmem_wdata   = rs2_reg;
                dmem_byte_en = 4'b1111;
            end
            default: begin
                dmem_wdata   = 32'd0;
                dmem_byte_en = 4'b0000;
            end
        endcase
    end

    // Load data: extract and sign/zero extend from full word
    always_comb begin
        mem_load_data = dmem_rdata;
        case (dec_opcode)
            OP_LDB: begin
                case (alu_out_reg[1:0])
                    2'd0: mem_load_data = {{24{dmem_rdata[7]}},  dmem_rdata[7:0]};
                    2'd1: mem_load_data = {{24{dmem_rdata[15]}}, dmem_rdata[15:8]};
                    2'd2: mem_load_data = {{24{dmem_rdata[23]}}, dmem_rdata[23:16]};
                    2'd3: mem_load_data = {{24{dmem_rdata[31]}}, dmem_rdata[31:24]};
                endcase
            end
            OP_LDBU: begin
                case (alu_out_reg[1:0])
                    2'd0: mem_load_data = {24'd0, dmem_rdata[7:0]};
                    2'd1: mem_load_data = {24'd0, dmem_rdata[15:8]};
                    2'd2: mem_load_data = {24'd0, dmem_rdata[23:16]};
                    2'd3: mem_load_data = {24'd0, dmem_rdata[31:24]};
                endcase
            end
            OP_LDH: begin
                case (alu_out_reg[1])
                    1'd0: mem_load_data = {{16{dmem_rdata[15]}}, dmem_rdata[15:0]};
                    1'd1: mem_load_data = {{16{dmem_rdata[31]}}, dmem_rdata[31:16]};
                endcase
            end
            OP_LDHU: begin
                case (alu_out_reg[1])
                    1'd0: mem_load_data = {16'd0, dmem_rdata[15:0]};
                    1'd1: mem_load_data = {16'd0, dmem_rdata[31:16]};
                endcase
            end
            OP_LDW: begin
                mem_load_data = dmem_rdata;
            end
            default: begin
                mem_load_data = dmem_rdata;
            end
        endcase
    end

    // ========================================================================
    // Instruction fetch address
    // ========================================================================
    assign imem_addr = pc;

    // ========================================================================
    // Main FSM
    // ========================================================================
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state       <= ST_INIT_SP;
            pc          <= 32'd0;
            ir          <= 32'd0;
            rs1_reg     <= 32'd0;
            rs2_reg     <= 32'd0;
            alu_out_reg <= 32'd0;
            halted      <= 1'b0;
            debug_valid <= 1'b0;
            debug_char  <= 8'd0;
            assert_fail <= 1'b0;
            assert_rs1  <= 5'd0;
            assert_rs2  <= 5'd0;
            assert_val1 <= 32'd0;
            assert_val2 <= 32'd0;
            assert_pc   <= 32'd0;
            rf_wr_en    <= 1'b0;
            rf_wr_addr  <= 5'd0;
            rf_wr_data  <= 32'd0;
            dmem_wen    <= 1'b0;
            dmem_ren    <= 1'b0;
        end else if (!halted) begin
            // Default: clear single-cycle pulses
            debug_valid <= 1'b0;
            assert_fail <= 1'b0;
            rf_wr_en    <= 1'b0;
            dmem_wen    <= 1'b0;
            dmem_ren    <= 1'b0;

            case (state)
                // ============================================================
                // INIT_SP: Write init_sp to r29 (SP), then init FP
                // ============================================================
                ST_INIT_SP: begin
                    rf_wr_en   <= 1'b1;
                    rf_wr_addr <= 5'd29;
                    rf_wr_data <= init_sp;
                    state      <= ST_INIT_FP;
                end

                // ============================================================
                // INIT_FP: Write init_sp to r30 (FP), set PC, enter FETCH
                // ============================================================
                ST_INIT_FP: begin
                    rf_wr_en   <= 1'b1;
                    rf_wr_addr <= 5'd30;
                    rf_wr_data <= init_sp;
                    pc         <= init_pc;
                    state      <= ST_FETCH;
                end

                // ============================================================
                // FETCH: Latch instruction from imem_rdata
                // ============================================================
                ST_FETCH: begin
                    ir    <= imem_rdata;
                    state <= ST_DECODE;
                end

                // ============================================================
                // DECODE: Latch register values for EXECUTE
                // ============================================================
                ST_DECODE: begin
                    rs1_reg <= rs1_val;
                    rs2_reg <= rs2_val;
                    state   <= ST_EXECUTE;
                end

                // ============================================================
                // EXECUTE: Run ALU, evaluate branches, handle jumps
                // ============================================================
                ST_EXECUTE: begin
                    alu_out_reg <= alu_result;

                    if (is_load) begin
                        dmem_ren <= 1'b1;
                        state    <= ST_MEMORY;
                    end else if (is_store) begin
                        dmem_wen <= 1'b1;
                        state    <= ST_MEMORY;
                    end else if (is_branch) begin
                        if (branch_taken)
                            pc <= pc + 32'd4 + dec_imm_b;
                        else
                            pc <= pc + 32'd4;
                        state <= ST_FETCH;
                    end else if (is_jal) begin
                        rf_wr_en   <= 1'b1;
                        rf_wr_addr <= dec_rd;
                        rf_wr_data <= pc + 32'd4;
                        pc         <= pc + dec_imm_j;
                        state      <= ST_FETCH;
                    end else if (is_jalr) begin
                        rf_wr_en   <= 1'b1;
                        rf_wr_addr <= dec_rd;
                        rf_wr_data <= pc + 32'd4;
                        pc         <= alu_result & ~32'd1;
                        state      <= ST_FETCH;
                    end else if (dec_opcode == OP_DEBUG) begin
                        debug_valid <= 1'b1;
                        debug_char  <= rs1_reg[7:0];
                        pc          <= pc + 32'd4;
                        state       <= ST_FETCH;
                    end else if (dec_opcode == OP_HALT) begin
                        halted <= 1'b1;
                        state  <= ST_FETCH;
                    end else if (dec_opcode == OP_NOP || dec_opcode == OP_YIELD) begin
                        pc    <= pc + 32'd4;
                        state <= ST_FETCH;
                    end else if (dec_opcode == OP_ASSERT_EQ) begin
                        if (rs1_reg != rs2_reg) begin
                            assert_fail <= 1'b1;
                            assert_rs1  <= dec_rs1;
                            assert_rs2  <= dec_rs2;
                            assert_val1 <= rs1_reg;
                            assert_val2 <= rs2_reg;
                            assert_pc   <= pc;
                            halted      <= 1'b1;
                        end
                        pc    <= pc + 32'd4;
                        state <= ST_FETCH;
                    end else begin
                        // ALU instruction: go to WRITEBACK
                        state <= ST_WRITEBACK;
                    end
                end

                // ============================================================
                // MEMORY: Wait one cycle for memory response
                // ============================================================
                ST_MEMORY: begin
                    dmem_wen <= 1'b0;
                    dmem_ren <= 1'b0;
                    if (is_load) begin
                        state <= ST_WRITEBACK;
                    end else begin
                        pc    <= pc + 32'd4;
                        state <= ST_FETCH;
                    end
                end

                // ============================================================
                // WRITEBACK: Write result to register file, advance PC
                // ============================================================
                ST_WRITEBACK: begin
                    rf_wr_en   <= 1'b1;
                    rf_wr_addr <= dec_rd;
                    if (is_load)
                        rf_wr_data <= mem_load_data;
                    else
                        rf_wr_data <= alu_out_reg;
                    pc    <= pc + 32'd4;
                    state <= ST_FETCH;
                end

                default: state <= ST_FETCH;
            endcase
        end
    end

endmodule
