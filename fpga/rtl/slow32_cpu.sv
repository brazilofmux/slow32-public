// slow32_cpu.sv — 5-stage in-order pipelined SLOW-32 CPU
//
// Stages: IF → ID → EX → MEM → WB
// Hazard handling:
//   - Data forwarding: EX/MEM → EX, MEM/WB → EX
//   - Load-use stall: 1-cycle bubble when EX is a load and ID needs the result
//   - Divide stall: 32-cycle stall while divider runs
//   - Branch/jump flush: 2-cycle penalty (flush IF/ID and ID/EX)
// Write-through register file handles WB→ID same-cycle bypass.

import slow32_pkg::*;

module slow32_cpu (
    input  logic        clk,
    input  logic        rst_n,

    // Initialization inputs (latched on reset release)
    input  logic [31:0] init_pc,
    input  logic [31:0] init_sp,

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

    // Cache ready signals
    input  logic        imem_ready,    // I-cache: instruction data valid
    input  logic        dmem_ready,    // D-cache: load/store data valid

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
    output logic [31:0] assert_pc,

    // Branch predictor → I-cache redirect
    output logic        bp_redirect,
    output logic [31:0] bp_redirect_addr
);

    // ========================================================================
    // Init state machine (2-cycle init, then pipeline runs)
    // ========================================================================
    cpu_state_t cpu_state;
    logic       running;
    assign running = (cpu_state == ST_RUNNING);

    // ========================================================================
    // PC
    // ========================================================================
    logic [31:0] pc;

    // ========================================================================
    // Pipeline control signals
    // ========================================================================
    logic stall_if, stall_id, stall_ex;
    logic flush_if_id, flush_id_ex;

    // ========================================================================
    // Divider signals
    // ========================================================================
    logic        div_start;
    logic        div_done;
    logic [31:0] div_dividend, div_divisor;
    logic [31:0] div_quotient, div_remainder;
    logic        div_busy;       // Divider is running
    logic        div_sign_q;     // Negate quotient?
    logic        div_sign_r;     // Negate remainder?
    logic        div_is_rem;     // REM (not DIV)?
    logic [4:0]  div_rd;         // Saved destination register

    // ========================================================================
    // IF/ID pipeline register
    // ========================================================================
    logic        if_id_valid;
    logic [31:0] if_id_pc;
    logic [31:0] if_id_ir;

    // ========================================================================
    // Decoder outputs (combinational, driven from if_id_ir)
    // ========================================================================
    logic [6:0]  dec_opcode;
    logic [4:0]  dec_rd, dec_rs1, dec_rs2;
    logic [31:0] dec_imm_i, dec_imm_iz, dec_imm_s, dec_imm_b, dec_imm_u, dec_imm_j;

    // ========================================================================
    // Register file signals
    // ========================================================================
    logic [31:0] rs1_val, rs2_val;
    logic        rf_wr_en;
    logic [4:0]  rf_wr_addr;
    logic [31:0] rf_wr_data;

    // ========================================================================
    // ID/EX pipeline register
    // ========================================================================
    logic        id_ex_valid;
    logic [31:0] id_ex_pc;
    logic [4:0]  id_ex_rs1, id_ex_rs2, id_ex_rd;
    logic [31:0] id_ex_rs1_val, id_ex_rs2_val;
    logic [6:0]  id_ex_opcode;
    alu_op_t     id_ex_alu_op;
    logic [31:0] id_ex_imm_i, id_ex_imm_iz, id_ex_imm_s, id_ex_imm_b, id_ex_imm_u, id_ex_imm_j;
    logic        id_ex_is_load, id_ex_is_store, id_ex_is_branch;
    logic        id_ex_is_jal, id_ex_is_jalr, id_ex_is_div;
    logic        id_ex_rf_wr_en;
    logic        id_ex_id_redirected;  // This instruction was predicted at ID stage

    // ========================================================================
    // EX/MEM pipeline register
    // ========================================================================
    logic        ex_mem_valid;
    logic [31:0] ex_mem_alu_result;
    logic [31:0] ex_mem_rf_wr_data;
    logic [31:0] ex_mem_store_data;
    logic [4:0]  ex_mem_rd;
    logic [6:0]  ex_mem_opcode;
    logic        ex_mem_is_load, ex_mem_is_store;
    logic        ex_mem_rf_wr_en;

    // ========================================================================
    // MEM/WB pipeline register
    // ========================================================================
    logic        mem_wb_valid;
    logic [31:0] mem_wb_rf_wr_data;
    logic [4:0]  mem_wb_rd;
    logic        mem_wb_rf_wr_en;

    // ========================================================================
    // Submodule instantiation
    // ========================================================================

    // Decoder — driven from IF/ID instruction register
    slow32_decode u_decode (
        .instr   (if_id_ir),
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

    // Register file (write-through bypass built in)
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
    alu_op_t     alu_op;
    logic [31:0] alu_a, alu_b, alu_result;

    slow32_alu u_alu (
        .op     (alu_op),
        .a      (alu_a),
        .b      (alu_b),
        .result (alu_result)
    );

    // Divider
    slow32_divider u_divider (
        .clk       (clk),
        .rst_n     (rst_n),
        .start     (div_start),
        .dividend  (div_dividend),
        .divisor   (div_divisor),
        .done      (div_done),
        .quotient  (div_quotient),
        .remainder (div_remainder)
    );

    // ========================================================================
    // ID stage: instruction classification (combinational from decoder output)
    // ========================================================================
    logic id_is_load, id_is_store, id_is_branch, id_is_jal, id_is_jalr, id_is_div;
    logic id_rf_wr_en;

    always_comb begin
        id_is_load   = (dec_opcode >= OP_LDB && dec_opcode <= OP_LDHU);
        id_is_store  = (dec_opcode >= OP_STB && dec_opcode <= OP_STW);
        id_is_jal    = (dec_opcode == OP_JAL);
        id_is_jalr   = (dec_opcode == OP_JALR);
        id_is_branch = (dec_opcode >= OP_BEQ && dec_opcode <= OP_BGEU);
        id_is_div    = (dec_opcode == OP_DIV || dec_opcode == OP_REM);
    end

    // Will this instruction write a register?
    always_comb begin
        id_rf_wr_en = 1'b0;
        if (if_id_valid) begin
            case (dec_opcode)
                OP_ADD, OP_SUB, OP_XOR, OP_OR, OP_AND,
                OP_SLL, OP_SRL, OP_SRA, OP_SLT, OP_SLTU,
                OP_MUL, OP_MULH, OP_DIV, OP_REM,
                OP_SEQ, OP_SNE, OP_MULHU:
                    id_rf_wr_en = 1'b1;
                OP_ADDI, OP_ORI, OP_ANDI, OP_SLLI, OP_SRLI, OP_SRAI,
                OP_SLTI, OP_SLTIU, OP_XORI:
                    id_rf_wr_en = 1'b1;
                OP_SGT, OP_SGTU, OP_SLE, OP_SLEU, OP_SGE, OP_SGEU:
                    id_rf_wr_en = 1'b1;
                OP_LUI:
                    id_rf_wr_en = 1'b1;
                OP_LDB, OP_LDH, OP_LDW, OP_LDBU, OP_LDHU:
                    id_rf_wr_en = 1'b1;
                OP_JAL, OP_JALR:
                    id_rf_wr_en = 1'b1;
                default:
                    id_rf_wr_en = 1'b0;
            endcase
        end
    end

    // ========================================================================
    // ID stage: branch prediction (JAL early resolve + BTFNT)
    // ========================================================================
    logic [31:0] id_branch_target;
    logic        id_predict_redirect;

    always_comb begin
        id_branch_target    = 32'd0;
        id_predict_redirect = 1'b0;

        if (if_id_valid && !stall_id && !flush_id_ex) begin
            if (id_is_jal) begin
                // JAL: always taken, resolve at ID
                id_predict_redirect = 1'b1;
                id_branch_target    = if_id_pc + dec_imm_j;
            end else if (id_is_branch && dec_imm_b[31]) begin
                // Backward branch: predict taken (BTFNT)
                id_predict_redirect = 1'b1;
                id_branch_target    = if_id_pc + 32'd4 + dec_imm_b;
            end
        end
    end

    // ALU op selection (combinational from decoder output)
    alu_op_t id_alu_op;
    always_comb begin
        id_alu_op = ALU_ADD;
        case (dec_opcode)
            OP_ADD:   id_alu_op = ALU_ADD;
            OP_SUB:   id_alu_op = ALU_SUB;
            OP_XOR:   id_alu_op = ALU_XOR;
            OP_OR:    id_alu_op = ALU_OR;
            OP_AND:   id_alu_op = ALU_AND;
            OP_SLL:   id_alu_op = ALU_SLL;
            OP_SRL:   id_alu_op = ALU_SRL;
            OP_SRA:   id_alu_op = ALU_SRA;
            OP_SLT:   id_alu_op = ALU_SLT;
            OP_SLTU:  id_alu_op = ALU_SLTU;
            OP_MUL:   id_alu_op = ALU_MUL;
            OP_MULH:  id_alu_op = ALU_MULH;
            OP_DIV:   id_alu_op = ALU_DIV;
            OP_REM:   id_alu_op = ALU_REM;
            OP_SEQ:   id_alu_op = ALU_SEQ;
            OP_SNE:   id_alu_op = ALU_SNE;
            OP_ADDI:  id_alu_op = ALU_ADD;
            OP_ORI:   id_alu_op = ALU_OR;
            OP_ANDI:  id_alu_op = ALU_AND;
            OP_SLLI:  id_alu_op = ALU_SLL;
            OP_SRLI:  id_alu_op = ALU_SRL;
            OP_SRAI:  id_alu_op = ALU_SRA;
            OP_SLTI:  id_alu_op = ALU_SLT;
            OP_SLTIU: id_alu_op = ALU_SLTU;
            OP_XORI:  id_alu_op = ALU_XOR;
            OP_SGT:   id_alu_op = ALU_SGT;
            OP_SGTU:  id_alu_op = ALU_SGTU;
            OP_SLE:   id_alu_op = ALU_SLE;
            OP_SLEU:  id_alu_op = ALU_SLEU;
            OP_SGE:   id_alu_op = ALU_SGE;
            OP_SGEU:  id_alu_op = ALU_SGEU;
            OP_MULHU: id_alu_op = ALU_MULHU;
            OP_LUI:   id_alu_op = ALU_PASS_B;
            OP_LDB, OP_LDH, OP_LDW, OP_LDBU, OP_LDHU: id_alu_op = ALU_ADD;
            OP_STB, OP_STH, OP_STW:                     id_alu_op = ALU_ADD;
            OP_JAL, OP_JALR: id_alu_op = ALU_ADD;
            default: id_alu_op = ALU_ADD;
        endcase
    end

    // ========================================================================
    // Hazard detection (combinational)
    // ========================================================================

    // Load-use hazard: instruction in EX is a load, and instruction in ID
    // reads the load's destination register
    logic load_use_hazard;
    always_comb begin
        load_use_hazard = 1'b0;
        if (id_ex_valid && id_ex_is_load && id_ex_rd != 5'd0) begin
            if (if_id_valid) begin
                // Check rs1 usage
                if (dec_rs1 == id_ex_rd) begin
                    // Almost all instructions use rs1
                    case (dec_opcode)
                        OP_LUI, OP_JAL, OP_NOP, OP_YIELD, OP_HALT:
                            ;  // These don't use rs1
                        default:
                            load_use_hazard = 1'b1;
                    endcase
                end
                // Check rs2 usage (R-type, stores, branches)
                if (dec_rs2 == id_ex_rd) begin
                    case (dec_opcode)
                        OP_ADD, OP_SUB, OP_XOR, OP_OR, OP_AND,
                        OP_SLL, OP_SRL, OP_SRA, OP_SLT, OP_SLTU,
                        OP_MUL, OP_MULH, OP_DIV, OP_REM,
                        OP_SEQ, OP_SNE, OP_MULHU,
                        OP_SGT, OP_SGTU, OP_SLE, OP_SLEU, OP_SGE, OP_SGEU:
                            load_use_hazard = 1'b1;
                        OP_STB, OP_STH, OP_STW:
                            load_use_hazard = 1'b1;
                        OP_BEQ, OP_BNE, OP_BLT, OP_BGE, OP_BLTU, OP_BGEU:
                            load_use_hazard = 1'b1;
                        OP_ASSERT_EQ:
                            load_use_hazard = 1'b1;
                        default: ;
                    endcase
                end
            end
        end
    end

    // Divide stall: divider is busy
    logic divide_stall;
    assign divide_stall = div_busy;

    // Cache stalls: freeze entire pipeline when cache not ready
    logic icache_stall, dcache_stall;
    assign icache_stall = running && !imem_ready;
    assign dcache_stall = running && ex_mem_valid && (ex_mem_is_load || ex_mem_is_store) && !dmem_ready;

    // Combined stall signals
    // Load-use: freeze IF only; EX advances (load → MEM), ID inserts bubble via flush_id_ex
    // Divide: freeze IF, ID, and EX
    // Cache stalls: freeze entire pipeline (same as divide stall)
    assign stall_ex = divide_stall || icache_stall || dcache_stall;
    assign stall_id = stall_ex;
    assign stall_if = load_use_hazard || stall_ex;

    // ========================================================================
    // EX stage: forwarding logic (combinational)
    // ========================================================================
    logic [31:0] fwd_rs1, fwd_rs2;

    always_comb begin
        // rs1 forwarding
        if (ex_mem_valid && ex_mem_rf_wr_en && ex_mem_rd != 5'd0 &&
            ex_mem_rd == id_ex_rs1)
            fwd_rs1 = ex_mem_rf_wr_data;
        else if (mem_wb_valid && mem_wb_rf_wr_en && mem_wb_rd != 5'd0 &&
                 mem_wb_rd == id_ex_rs1)
            fwd_rs1 = mem_wb_rf_wr_data;
        else
            fwd_rs1 = id_ex_rs1_val;

        // rs2 forwarding
        if (ex_mem_valid && ex_mem_rf_wr_en && ex_mem_rd != 5'd0 &&
            ex_mem_rd == id_ex_rs2)
            fwd_rs2 = ex_mem_rf_wr_data;
        else if (mem_wb_valid && mem_wb_rf_wr_en && mem_wb_rd != 5'd0 &&
                 mem_wb_rd == id_ex_rs2)
            fwd_rs2 = mem_wb_rf_wr_data;
        else
            fwd_rs2 = id_ex_rs2_val;
    end

    // ========================================================================
    // EX stage: ALU operand selection (combinational, uses forwarded values)
    // ========================================================================
    always_comb begin
        alu_op = id_ex_alu_op;
        alu_a  = fwd_rs1;
        alu_b  = fwd_rs2;

        case (id_ex_opcode)
            // I-type with sign-extended immediate
            OP_ADDI, OP_SLLI, OP_SRLI, OP_SRAI, OP_SLTI:
                alu_b = id_ex_imm_i;

            // I-type with zero-extended immediate
            OP_ORI, OP_ANDI, OP_XORI, OP_SLTIU:
                alu_b = id_ex_imm_iz;

            // LUI: pass U-type immediate
            OP_LUI: begin
                alu_a = 32'd0;
                alu_b = id_ex_imm_u;
            end

            // Loads: rs1 + sign-extended I-type offset
            OP_LDB, OP_LDH, OP_LDW, OP_LDBU, OP_LDHU:
                alu_b = id_ex_imm_i;

            // Stores: rs1 + sign-extended S-type offset
            OP_STB, OP_STH, OP_STW:
                alu_b = id_ex_imm_s;

            // JALR: rs1 + imm_i
            OP_JALR:
                alu_b = id_ex_imm_i;

            default: begin
                alu_a = fwd_rs1;
                alu_b = fwd_rs2;
            end
        endcase
    end

    // ========================================================================
    // EX stage: branch evaluation (combinational, uses forwarded values)
    // ========================================================================
    logic branch_taken;
    logic [31:0] branch_target;
    logic ex_redirect;  // Take a branch/jump, flush pipeline

    always_comb begin
        branch_taken = 1'b0;
        if (id_ex_valid && id_ex_is_branch) begin
            case (id_ex_opcode)
                OP_BEQ:  branch_taken = (fwd_rs1 == fwd_rs2);
                OP_BNE:  branch_taken = (fwd_rs1 != fwd_rs2);
                OP_BLT:  branch_taken = ($signed(fwd_rs1) <  $signed(fwd_rs2));
                OP_BGE:  branch_taken = ($signed(fwd_rs1) >= $signed(fwd_rs2));
                OP_BLTU: branch_taken = (fwd_rs1 <  fwd_rs2);
                OP_BGEU: branch_taken = (fwd_rs1 >= fwd_rs2);
                default: branch_taken = 1'b0;
            endcase
        end
    end

    always_comb begin
        branch_target = 32'd0;
        if (id_ex_id_redirected && id_ex_is_branch && !branch_taken)
            branch_target = id_ex_pc + 32'd4;                  // Mispredict: fall-through
        else if (id_ex_is_branch)
            branch_target = id_ex_pc + 32'd4 + id_ex_imm_b;   // Branch target
        else if (id_ex_is_jal)
            branch_target = id_ex_pc + id_ex_imm_j;            // JAL target
        else if (id_ex_is_jalr)
            branch_target = alu_result & ~32'd1;               // JALR target
    end

    always_comb begin
        ex_redirect = 1'b0;
        if (id_ex_valid && !stall_ex) begin
            if (id_ex_id_redirected) begin
                // Was predicted at ID — validate
                // JAL: always correct, no EX redirect needed
                // Backward branch predicted taken: check actual condition
                if (id_ex_is_branch && !branch_taken) begin
                    // MISPREDICT: predicted taken, actually not taken
                    ex_redirect = 1'b1;
                end
            end else begin
                // Not predicted — handle normally
                if ((id_ex_is_branch && branch_taken) || id_ex_is_jalr)
                    ex_redirect = 1'b1;
                if (id_ex_opcode == OP_HALT)
                    ex_redirect = 1'b1;  // Flush pipeline on halt
            end
        end
    end

    // Flush IF/ID when EX redirects OR ID predicts a branch
    // Flush ID/EX when EX redirects OR load-use hazard (NOT on ID prediction — branch must advance to EX)
    assign flush_if_id = ex_redirect || id_predict_redirect;
    assign flush_id_ex = ex_redirect || load_use_hazard;

    // ========================================================================
    // EX stage: compute writeback data for this instruction
    // ========================================================================
    logic [31:0] ex_wr_data;
    logic        ex_is_div_corner;
    logic [31:0] ex_div_corner_result;

    // DIV/REM corner case detection
    always_comb begin
        ex_is_div_corner = 1'b0;
        ex_div_corner_result = 32'd0;
        if (id_ex_valid && id_ex_is_div) begin
            if (fwd_rs2 == 32'd0) begin
                // Div-by-zero
                ex_is_div_corner = 1'b1;
                ex_div_corner_result = (id_ex_opcode == OP_REM) ? fwd_rs1 : 32'hFFFFFFFF;
            end else if (fwd_rs1 == 32'h80000000 && fwd_rs2 == 32'hFFFFFFFF) begin
                // INT_MIN / -1
                ex_is_div_corner = 1'b1;
                ex_div_corner_result = (id_ex_opcode == OP_REM) ? 32'd0 : 32'h80000000;
            end
        end
    end

    always_comb begin
        if (id_ex_is_jal || id_ex_is_jalr)
            ex_wr_data = id_ex_pc + 32'd4;  // Link address
        else if (id_ex_is_div && ex_is_div_corner)
            ex_wr_data = ex_div_corner_result;
        else
            ex_wr_data = alu_result;
    end

    // ========================================================================
    // MEM stage: memory interface (active when EX/MEM has a load or store)
    // ========================================================================
    assign dmem_addr = ex_mem_alu_result;

    // dmem_ren/dmem_wen
    assign dmem_ren = ex_mem_valid && ex_mem_is_load;
    assign dmem_wen = ex_mem_valid && ex_mem_is_store;

    // Store data: position on correct byte lanes
    always_comb begin
        dmem_wdata   = 32'd0;
        dmem_byte_en = 4'b0000;
        if (ex_mem_valid && ex_mem_is_store) begin
            case (ex_mem_opcode)
                OP_STB: begin
                    dmem_wdata = {4{ex_mem_store_data[7:0]}};
                    case (ex_mem_alu_result[1:0])
                        2'd0: dmem_byte_en = 4'b0001;
                        2'd1: dmem_byte_en = 4'b0010;
                        2'd2: dmem_byte_en = 4'b0100;
                        2'd3: dmem_byte_en = 4'b1000;
                    endcase
                end
                OP_STH: begin
                    dmem_wdata = {2{ex_mem_store_data[15:0]}};
                    case (ex_mem_alu_result[1])
                        1'd0: dmem_byte_en = 4'b0011;
                        1'd1: dmem_byte_en = 4'b1100;
                    endcase
                end
                OP_STW: begin
                    dmem_wdata   = ex_mem_store_data;
                    dmem_byte_en = 4'b1111;
                end
                default: begin
                    dmem_wdata   = 32'd0;
                    dmem_byte_en = 4'b0000;
                end
            endcase
        end
    end

    // Load data: extract and sign/zero extend from full word
    logic [31:0] mem_load_data;
    always_comb begin
        mem_load_data = dmem_rdata;
        case (ex_mem_opcode)
            OP_LDB: begin
                case (ex_mem_alu_result[1:0])
                    2'd0: mem_load_data = {{24{dmem_rdata[7]}},  dmem_rdata[7:0]};
                    2'd1: mem_load_data = {{24{dmem_rdata[15]}}, dmem_rdata[15:8]};
                    2'd2: mem_load_data = {{24{dmem_rdata[23]}}, dmem_rdata[23:16]};
                    2'd3: mem_load_data = {{24{dmem_rdata[31]}}, dmem_rdata[31:24]};
                endcase
            end
            OP_LDBU: begin
                case (ex_mem_alu_result[1:0])
                    2'd0: mem_load_data = {24'd0, dmem_rdata[7:0]};
                    2'd1: mem_load_data = {24'd0, dmem_rdata[15:8]};
                    2'd2: mem_load_data = {24'd0, dmem_rdata[23:16]};
                    2'd3: mem_load_data = {24'd0, dmem_rdata[31:24]};
                endcase
            end
            OP_LDH: begin
                case (ex_mem_alu_result[1])
                    1'd0: mem_load_data = {{16{dmem_rdata[15]}}, dmem_rdata[15:0]};
                    1'd1: mem_load_data = {{16{dmem_rdata[31]}}, dmem_rdata[31:16]};
                endcase
            end
            OP_LDHU: begin
                case (ex_mem_alu_result[1])
                    1'd0: mem_load_data = {16'd0, dmem_rdata[15:0]};
                    1'd1: mem_load_data = {16'd0, dmem_rdata[31:16]};
                endcase
            end
            OP_LDW:
                mem_load_data = dmem_rdata;
            default:
                mem_load_data = dmem_rdata;
        endcase
    end

    // ========================================================================
    // WB stage: register file write
    // ========================================================================
    assign rf_wr_en   = mem_wb_valid && mem_wb_rf_wr_en && (mem_wb_rd != 5'd0);
    assign rf_wr_addr = mem_wb_rd;
    assign rf_wr_data = mem_wb_rf_wr_data;

    // ========================================================================
    // Branch predictor → I-cache redirect
    // ========================================================================
    // EX redirect has priority (mispredict correction overrides ID prediction)
    assign bp_redirect      = ex_redirect || id_predict_redirect;
    assign bp_redirect_addr = ex_redirect ? branch_target : id_branch_target;

    // ========================================================================
    // Instruction fetch address
    // ========================================================================
    assign imem_addr = pc;

    // ========================================================================
    // EX stage: DEBUG instruction
    // ========================================================================
    logic ex_debug_valid;
    logic [7:0] ex_debug_char;
    always_comb begin
        ex_debug_valid = id_ex_valid && !stall_ex && (id_ex_opcode == OP_DEBUG);
        ex_debug_char  = fwd_rs1[7:0];
    end

    // ========================================================================
    // EX stage: ASSERT_EQ instruction
    // ========================================================================
    logic ex_assert_fail;
    always_comb begin
        ex_assert_fail = 1'b0;
        if (id_ex_valid && !stall_ex && id_ex_opcode == OP_ASSERT_EQ) begin
            if (fwd_rs1 != fwd_rs2)
                ex_assert_fail = 1'b1;
        end
    end

    // ========================================================================
    // Main pipeline clock logic
    // ========================================================================
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            cpu_state    <= ST_INIT_SP;
            pc           <= 32'd0;
            halted       <= 1'b0;
            debug_valid  <= 1'b0;
            debug_char   <= 8'd0;
            assert_fail  <= 1'b0;
            assert_rs1   <= 5'd0;
            assert_rs2   <= 5'd0;
            assert_val1  <= 32'd0;
            assert_val2  <= 32'd0;
            assert_pc    <= 32'd0;

            // Pipeline registers
            if_id_valid  <= 1'b0;
            if_id_pc     <= 32'd0;
            if_id_ir     <= 32'd0;

            id_ex_valid  <= 1'b0;
            id_ex_pc     <= 32'd0;
            id_ex_rs1    <= 5'd0;
            id_ex_rs2    <= 5'd0;
            id_ex_rd     <= 5'd0;
            id_ex_rs1_val <= 32'd0;
            id_ex_rs2_val <= 32'd0;
            id_ex_opcode <= 7'd0;
            id_ex_alu_op <= ALU_ADD;
            id_ex_imm_i  <= 32'd0;
            id_ex_imm_iz <= 32'd0;
            id_ex_imm_s  <= 32'd0;
            id_ex_imm_b  <= 32'd0;
            id_ex_imm_u  <= 32'd0;
            id_ex_imm_j  <= 32'd0;
            id_ex_is_load  <= 1'b0;
            id_ex_is_store <= 1'b0;
            id_ex_is_branch <= 1'b0;
            id_ex_is_jal   <= 1'b0;
            id_ex_is_jalr  <= 1'b0;
            id_ex_is_div   <= 1'b0;
            id_ex_rf_wr_en <= 1'b0;
            id_ex_id_redirected <= 1'b0;

            ex_mem_valid     <= 1'b0;
            ex_mem_alu_result <= 32'd0;
            ex_mem_rf_wr_data <= 32'd0;
            ex_mem_store_data <= 32'd0;
            ex_mem_rd        <= 5'd0;
            ex_mem_opcode    <= 7'd0;
            ex_mem_is_load   <= 1'b0;
            ex_mem_is_store  <= 1'b0;
            ex_mem_rf_wr_en  <= 1'b0;

            mem_wb_valid     <= 1'b0;
            mem_wb_rf_wr_data <= 32'd0;
            mem_wb_rd        <= 5'd0;
            mem_wb_rf_wr_en  <= 1'b0;

            // Divider
            div_start    <= 1'b0;
            div_dividend <= 32'd0;
            div_divisor  <= 32'd0;
            div_busy     <= 1'b0;
            div_sign_q   <= 1'b0;
            div_sign_r   <= 1'b0;
            div_is_rem   <= 1'b0;
            div_rd       <= 5'd0;
        end else if (halted) begin
            // Stay halted, clear pulses
            debug_valid <= 1'b0;
            assert_fail <= 1'b0;
            div_start   <= 1'b0;
        end else begin
            // Default: clear single-cycle pulses
            debug_valid <= 1'b0;
            assert_fail <= 1'b0;
            div_start   <= 1'b0;

            case (cpu_state)
                // ============================================================
                // INIT_SP: Write init_sp to r29 (SP)
                // ============================================================
                ST_INIT_SP: begin
                    // Use the regfile write port directly via WB stage signals
                    // We'll set mem_wb to write r29
                    mem_wb_valid     <= 1'b1;
                    mem_wb_rf_wr_en  <= 1'b1;
                    mem_wb_rd        <= 5'd29;
                    mem_wb_rf_wr_data <= init_sp;
                    cpu_state        <= ST_INIT_FP;
                end

                // ============================================================
                // INIT_FP: Write init_sp to r30 (FP), set PC, start running
                // ============================================================
                ST_INIT_FP: begin
                    mem_wb_valid     <= 1'b1;
                    mem_wb_rf_wr_en  <= 1'b1;
                    mem_wb_rd        <= 5'd30;
                    mem_wb_rf_wr_data <= init_sp;
                    pc               <= init_pc;
                    cpu_state        <= ST_RUNNING;
                end

                // ============================================================
                // RUNNING: Pipeline operates
                // ============================================================
                ST_RUNNING: begin

                    // ---- WB stage: MEM/WB → register file (via assign) ----
                    // (rf_wr_en/addr/data driven by assigns above)

                    // ---- MEM stage: EX/MEM → MEM/WB ----
                    if (!stall_ex) begin
                        mem_wb_valid <= ex_mem_valid;
                        mem_wb_rd   <= ex_mem_rd;
                        mem_wb_rf_wr_en <= ex_mem_rf_wr_en;
                        if (ex_mem_valid && ex_mem_is_load)
                            mem_wb_rf_wr_data <= mem_load_data;
                        else
                            mem_wb_rf_wr_data <= ex_mem_rf_wr_data;
                    end

                    // ---- EX stage: ID/EX → EX/MEM ----
                    if (!stall_ex) begin
                        // Check for HALT
                        if (id_ex_valid && id_ex_opcode == OP_HALT) begin
                            halted <= 1'b1;
                            // Flush everything downstream
                            ex_mem_valid <= 1'b0;
                        end
                        // Check for ASSERT failure
                        else if (ex_assert_fail) begin
                            assert_fail <= 1'b1;
                            assert_rs1  <= id_ex_rs1;
                            assert_rs2  <= id_ex_rs2;
                            assert_val1 <= fwd_rs1;
                            assert_val2 <= fwd_rs2;
                            assert_pc   <= id_ex_pc;
                            halted      <= 1'b1;
                            ex_mem_valid <= 1'b0;
                        end
                        // Check for DEBUG
                        else if (ex_debug_valid) begin
                            debug_valid <= 1'b1;
                            debug_char  <= ex_debug_char;
                            // DEBUG doesn't write back or access memory
                            ex_mem_valid    <= 1'b0;
                        end
                        // DIV/REM: start divider or handle corner case
                        else if (id_ex_valid && id_ex_is_div && !ex_is_div_corner && !div_busy) begin
                            // Start divider for normal case
                            // Save DIV context — ID/EX will be overwritten next cycle
                            // (div_busy isn't set until next edge, so ID advances this cycle)
                            div_sign_q  <= fwd_rs1[31] ^ fwd_rs2[31];
                            div_sign_r  <= fwd_rs1[31];
                            div_is_rem  <= (id_ex_opcode == OP_REM);
                            div_rd      <= id_ex_rd;
                            div_dividend <= fwd_rs1[31] ? (~fwd_rs1 + 32'd1) : fwd_rs1;
                            div_divisor  <= fwd_rs2[31] ? (~fwd_rs2 + 32'd1) : fwd_rs2;
                            div_start    <= 1'b1;
                            div_busy     <= 1'b1;
                            // Don't advance EX/MEM yet — result comes when div_done fires
                            ex_mem_valid <= 1'b0;
                        end
                        else begin
                            // Normal instruction: advance to EX/MEM
                            ex_mem_valid     <= id_ex_valid;
                            ex_mem_alu_result <= alu_result;
                            ex_mem_rf_wr_data <= ex_wr_data;
                            ex_mem_store_data <= fwd_rs2;
                            ex_mem_rd        <= id_ex_rd;
                            ex_mem_opcode    <= id_ex_opcode;
                            ex_mem_is_load   <= id_ex_is_load;
                            ex_mem_is_store  <= id_ex_is_store;
                            ex_mem_rf_wr_en  <= id_ex_rf_wr_en;
                            // Stores and branches don't write back
                            if (id_ex_is_store || id_ex_is_branch ||
                                id_ex_opcode == OP_NOP || id_ex_opcode == OP_YIELD)
                                ex_mem_rf_wr_en <= 1'b0;
                        end
                    end else if (divide_stall) begin
                        // Stalled on divider: check if divider just finished
                        if (div_done) begin
                            div_busy <= 1'b0;
                            // Latch divider result with sign correction
                            // Use saved div_rd (ID/EX was overwritten when divider started)
                            ex_mem_valid     <= 1'b1;
                            ex_mem_rd        <= div_rd;
                            ex_mem_opcode    <= 7'd0;
                            ex_mem_is_load   <= 1'b0;
                            ex_mem_is_store  <= 1'b0;
                            ex_mem_rf_wr_en  <= 1'b1;
                            ex_mem_alu_result <= 32'd0;
                            if (div_is_rem)
                                ex_mem_rf_wr_data <= div_sign_r ? (~div_remainder + 32'd1) : div_remainder;
                            else
                                ex_mem_rf_wr_data <= div_sign_q ? (~div_quotient + 32'd1) : div_quotient;
                            // Also need to push a bubble through MEM/WB since MEM was
                            // stalled and WB consumed the old MEM/WB last cycle
                            mem_wb_valid <= 1'b0;
                        end else begin
                            // Still waiting — push bubble through MEM/WB
                            mem_wb_valid <= ex_mem_valid;
                            mem_wb_rd    <= ex_mem_rd;
                            mem_wb_rf_wr_en <= ex_mem_rf_wr_en;
                            if (ex_mem_valid && ex_mem_is_load)
                                mem_wb_rf_wr_data <= mem_load_data;
                            else
                                mem_wb_rf_wr_data <= ex_mem_rf_wr_data;
                            ex_mem_valid <= 1'b0;
                        end
                    end
                    // else: cache stall — pipeline completely frozen, no register updates

                    // ---- ID stage: IF/ID → ID/EX ----
                    if (!stall_id) begin
                        if (flush_id_ex) begin
                            // Insert bubble
                            id_ex_valid <= 1'b0;
                        end else begin
                            id_ex_valid    <= if_id_valid;
                            id_ex_pc       <= if_id_pc;
                            id_ex_rs1      <= dec_rs1;
                            id_ex_rs2      <= dec_rs2;
                            id_ex_rd       <= dec_rd;
                            id_ex_rs1_val  <= rs1_val;
                            id_ex_rs2_val  <= rs2_val;
                            id_ex_opcode   <= dec_opcode;
                            id_ex_alu_op   <= id_alu_op;
                            id_ex_imm_i    <= dec_imm_i;
                            id_ex_imm_iz   <= dec_imm_iz;
                            id_ex_imm_s    <= dec_imm_s;
                            id_ex_imm_b    <= dec_imm_b;
                            id_ex_imm_u    <= dec_imm_u;
                            id_ex_imm_j    <= dec_imm_j;
                            id_ex_is_load  <= id_is_load;
                            id_ex_is_store <= id_is_store;
                            id_ex_is_branch <= id_is_branch;
                            id_ex_is_jal   <= id_is_jal;
                            id_ex_is_jalr  <= id_is_jalr;
                            id_ex_is_div   <= id_is_div;
                            id_ex_rf_wr_en <= id_rf_wr_en;
                            id_ex_id_redirected <= id_predict_redirect;
                        end
                    end
                    // else: stalled — hold ID/EX contents

                    // ---- IF stage: fetch → IF/ID ----
                    if (!stall_if) begin
                        if (flush_if_id) begin
                            // Insert bubble
                            if_id_valid <= 1'b0;
                        end else begin
                            if_id_valid <= 1'b1;
                            if_id_pc    <= pc;
                            if_id_ir    <= imem_rdata;
                        end

                        // Advance PC: EX redirect > ID prediction > sequential
                        if (ex_redirect)
                            pc <= branch_target;
                        else if (id_predict_redirect)
                            pc <= id_branch_target;
                        else
                            pc <= pc + 32'd4;
                    end
                    // else: stalled — hold PC and IF/ID contents

                end // ST_RUNNING
            endcase
        end
    end

endmodule
