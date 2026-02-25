// slow32_mem_arb.sv — Memory bus arbiter (D$ priority over I$)
//
// Muxes I-cache and D-cache requests onto a single memory bus.
// D-cache has priority. Locks the bus to one requester until it
// deasserts mem_req (end of transaction).

module slow32_mem_arb (
    input  logic        clk,
    input  logic        rst_n,

    // I-cache port (read-only)
    input  logic        ic_req,
    input  logic [31:0] ic_addr,
    output logic        ic_ready,
    output logic [31:0] ic_rdata,

    // D-cache port (read/write)
    input  logic        dc_req,
    input  logic        dc_wr,
    input  logic [31:0] dc_addr,
    input  logic [31:0] dc_wdata,
    input  logic [3:0]  dc_byte_en,
    output logic        dc_ready,
    output logic [31:0] dc_rdata,

    // External memory bus
    output logic        mem_req,
    output logic        mem_wr,
    output logic [31:0] mem_addr,
    output logic [31:0] mem_wdata,
    output logic [3:0]  mem_byte_en,
    input  logic        mem_ready,
    input  logic [31:0] mem_rdata
);

    // Bus ownership tracking
    typedef enum logic [1:0] {
        OWNER_NONE = 2'd0,
        OWNER_DC   = 2'd1,
        OWNER_IC   = 2'd2
    } owner_t;

    owner_t owner;

    // Determine current grant
    logic dc_grant, ic_grant;

    always_comb begin
        case (owner)
            OWNER_DC: begin
                // D$ owns the bus until it releases
                dc_grant = dc_req;
                ic_grant = !dc_req && ic_req;
            end
            OWNER_IC: begin
                // I$ owns the bus until it releases
                dc_grant = !ic_req && dc_req;
                ic_grant = ic_req;
            end
            default: begin
                // No owner: D$ has priority
                dc_grant = dc_req;
                ic_grant = !dc_req && ic_req;
            end
        endcase
    end

    // Mux to memory bus
    always_comb begin
        if (dc_grant) begin
            mem_req     = 1'b1;
            mem_wr      = dc_wr;
            mem_addr    = dc_addr;
            mem_wdata   = dc_wdata;
            mem_byte_en = dc_byte_en;
        end else if (ic_grant) begin
            mem_req     = 1'b1;
            mem_wr      = 1'b0;
            mem_addr    = ic_addr;
            mem_wdata   = 32'd0;
            mem_byte_en = 4'b1111;
        end else begin
            mem_req     = 1'b0;
            mem_wr      = 1'b0;
            mem_addr    = 32'd0;
            mem_wdata   = 32'd0;
            mem_byte_en = 4'b0000;
        end
    end

    // Route responses
    assign dc_ready = dc_grant && mem_ready;
    assign dc_rdata = mem_rdata;

    assign ic_ready = ic_grant && mem_ready;
    assign ic_rdata = mem_rdata;

    // Track ownership
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            owner <= OWNER_NONE;
        end else begin
            if (dc_grant)
                owner <= OWNER_DC;
            else if (ic_grant)
                owner <= OWNER_IC;
            else
                owner <= OWNER_NONE;
        end
    end

endmodule
