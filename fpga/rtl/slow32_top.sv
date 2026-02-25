// slow32_top.sv — Top-level: CPU + I$ + D$ + memory bus arbiter
//
// CPU instruction fetches go through I-cache.
// CPU data loads/stores go through D-cache (with write buffer).
// Both caches share a single memory bus via the arbiter (D$ priority).

import slow32_pkg::*;

module slow32_top (
    input  logic        clk,
    input  logic        rst_n,

    // Initialization inputs
    input  logic [31:0] init_pc,
    input  logic [31:0] init_sp,

    // External memory bus
    output logic        mem_req,
    output logic        mem_wr,
    output logic [31:0] mem_addr,
    output logic [31:0] mem_wdata,
    output logic [3:0]  mem_byte_en,
    input  logic        mem_ready,
    input  logic [31:0] mem_rdata,

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

    // CPU ↔ I-cache wires
    logic [31:0] cpu_imem_addr;
    logic [31:0] cpu_imem_rdata;
    logic        cpu_imem_ready;

    // Branch predictor redirect → I-cache
    logic        bp_redirect;
    logic [31:0] bp_redirect_addr;

    // CPU ↔ D-cache wires
    logic [31:0] cpu_dmem_addr;
    logic [31:0] cpu_dmem_wdata;
    logic [3:0]  cpu_dmem_byte_en;
    logic        cpu_dmem_wen;
    logic        cpu_dmem_ren;
    logic [31:0] cpu_dmem_rdata;
    logic        cpu_dmem_ready;

    // I-cache ↔ memory bus wires
    logic        ic_mem_req;
    logic [31:0] ic_mem_addr;
    logic        ic_mem_ready;
    logic [31:0] ic_mem_rdata;

    // D-cache ↔ memory bus wires
    logic        dc_mem_req;
    logic        dc_mem_wr;
    logic [31:0] dc_mem_addr;
    logic [31:0] dc_mem_wdata;
    logic [3:0]  dc_mem_byte_en;
    logic        dc_mem_ready;
    logic [31:0] dc_mem_rdata;

    // ========================================================================
    // CPU
    // ========================================================================
    slow32_cpu u_cpu (
        .clk        (clk),
        .rst_n      (rst_n),
        .init_pc    (init_pc),
        .init_sp    (init_sp),

        .imem_addr  (cpu_imem_addr),
        .imem_rdata (cpu_imem_rdata),

        .dmem_addr    (cpu_dmem_addr),
        .dmem_wdata   (cpu_dmem_wdata),
        .dmem_byte_en (cpu_dmem_byte_en),
        .dmem_wen     (cpu_dmem_wen),
        .dmem_ren     (cpu_dmem_ren),
        .dmem_rdata   (cpu_dmem_rdata),

        .imem_ready (cpu_imem_ready),
        .dmem_ready (cpu_dmem_ready),

        .halted      (halted),
        .debug_valid (debug_valid),
        .debug_char  (debug_char),
        .assert_fail (assert_fail),
        .assert_rs1  (assert_rs1),
        .assert_rs2  (assert_rs2),
        .assert_val1 (assert_val1),
        .assert_val2 (assert_val2),
        .assert_pc   (assert_pc),
        .bp_redirect      (bp_redirect),
        .bp_redirect_addr (bp_redirect_addr)
    );

    // ========================================================================
    // I-Cache
    // ========================================================================
    slow32_icache u_icache (
        .clk        (clk),
        .rst_n      (rst_n),
        .cpu_addr   (cpu_imem_addr),
        .cpu_rdata  (cpu_imem_rdata),
        .cpu_ready  (cpu_imem_ready),
        .redirect      (bp_redirect),
        .redirect_addr (bp_redirect_addr),
        .mem_req    (ic_mem_req),
        .mem_addr   (ic_mem_addr),
        .mem_ready  (ic_mem_ready),
        .mem_rdata  (ic_mem_rdata)
    );

    // ========================================================================
    // D-Cache
    // ========================================================================
    slow32_dcache u_dcache (
        .clk        (clk),
        .rst_n      (rst_n),
        .cpu_addr     (cpu_dmem_addr),
        .cpu_wdata    (cpu_dmem_wdata),
        .cpu_byte_en  (cpu_dmem_byte_en),
        .cpu_wen      (cpu_dmem_wen),
        .cpu_ren      (cpu_dmem_ren),
        .cpu_rdata    (cpu_dmem_rdata),
        .cpu_ready    (cpu_dmem_ready),
        .mem_req      (dc_mem_req),
        .mem_wr       (dc_mem_wr),
        .mem_addr     (dc_mem_addr),
        .mem_wdata    (dc_mem_wdata),
        .mem_byte_en  (dc_mem_byte_en),
        .mem_ready    (dc_mem_ready),
        .mem_rdata    (dc_mem_rdata)
    );

    // ========================================================================
    // Memory Bus Arbiter
    // ========================================================================
    slow32_mem_arb u_arb (
        .clk        (clk),
        .rst_n      (rst_n),
        .ic_req     (ic_mem_req),
        .ic_addr    (ic_mem_addr),
        .ic_ready   (ic_mem_ready),
        .ic_rdata   (ic_mem_rdata),
        .dc_req     (dc_mem_req),
        .dc_wr      (dc_mem_wr),
        .dc_addr    (dc_mem_addr),
        .dc_wdata   (dc_mem_wdata),
        .dc_byte_en (dc_mem_byte_en),
        .dc_ready   (dc_mem_ready),
        .dc_rdata   (dc_mem_rdata),
        .mem_req     (mem_req),
        .mem_wr      (mem_wr),
        .mem_addr    (mem_addr),
        .mem_wdata   (mem_wdata),
        .mem_byte_en (mem_byte_en),
        .mem_ready   (mem_ready),
        .mem_rdata   (mem_rdata)
    );

endmodule
