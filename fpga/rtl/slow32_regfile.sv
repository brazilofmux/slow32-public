// slow32_regfile.sv — 32x32-bit register file (2R/1W, r0 hardwired to zero)

module slow32_regfile (
    input  logic        clk,
    input  logic        rst_n,

    // Read port 1
    input  logic [4:0]  rs1_addr,
    output logic [31:0] rs1_data,

    // Read port 2
    input  logic [4:0]  rs2_addr,
    output logic [31:0] rs2_data,

    // Write port
    input  logic        wr_en,
    input  logic [4:0]  wr_addr,
    input  logic [31:0] wr_data
);

    logic [31:0] regs [1:31];  // r0 is hardwired to 0, not stored

    // Combinational reads (r0 = 0)
    assign rs1_data = (rs1_addr == 5'd0) ? 32'd0 : regs[rs1_addr];
    assign rs2_data = (rs2_addr == 5'd0) ? 32'd0 : regs[rs2_addr];

    // Synchronous write (writes to r0 are discarded)
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int i = 1; i < 32; i++)
                regs[i] <= 32'd0;
        end else if (wr_en && wr_addr != 5'd0) begin
            regs[wr_addr] <= wr_data;
        end
    end

endmodule
