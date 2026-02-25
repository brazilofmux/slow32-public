// slow32_icache.sv — 4KB direct-mapped instruction cache
//
// 256 lines × 16 bytes/line = 4KB. Direct-mapped, read-only.
// BRAM-based: 1-cycle read latency. Pipelined lookup with speculative
// prefetch hides latency in steady-state sequential fetches.
//
// Address decomposition (32-bit):
//   [31:12] tag (20 bits) | [11:4] index (8 bits) | [3:2] word (2 bits) | [1:0] byte
//
// Key idea: After delivering a hit for address X, speculatively read BRAM
// for X+4. If the CPU requests X+4 next cycle (sequential fetch), we have
// the data ready → 0 extra cycles. On branch redirect, re-read → +1 cycle.

module slow32_icache (
    input  logic        clk,
    input  logic        rst_n,

    // CPU side
    input  logic [31:0] cpu_addr,      // PC (driven from registered pc in CPU)
    output logic [31:0] cpu_rdata,     // Instruction word
    output logic        cpu_ready,     // 1 = instruction valid

    // Redirect feed-forward (from branch predictor / EX redirect)
    input  logic        redirect,      // Redirect this cycle
    input  logic [31:0] redirect_addr, // Target address to start reading

    // Memory bus side (for refills)
    output logic        mem_req,       // Request to memory bus
    output logic [31:0] mem_addr,      // Word address for refill
    input  logic        mem_ready,     // Memory bus ready
    input  logic [31:0] mem_rdata      // Read data from memory
);

    // ========================================================================
    // BRAM arrays
    // ========================================================================
    logic [20:0] tag_mem [0:255];      // {valid[20], tag[19:0]}
    logic [20:0] tag_rd;
    logic [127:0] data_mem [0:255];    // {word3, word2, word1, word0}
    logic [127:0] data_rd;

    // ========================================================================
    // Registered BRAM address: what we actually sent to BRAM last cycle
    // ========================================================================
    logic [31:0] bram_addr_reg;        // Address that BRAM output corresponds to
    logic        bram_addr_valid;      // BRAM output is usable

    logic [19:0] bram_tag;
    logic [7:0]  bram_index;
    logic [1:0]  bram_word;

    assign bram_tag   = bram_addr_reg[31:12];
    assign bram_index = bram_addr_reg[11:4];
    assign bram_word  = bram_addr_reg[3:2];

    // ========================================================================
    // Tag comparison (BRAM output vs registered BRAM address)
    // ========================================================================
    logic tag_valid;
    logic [19:0] tag_stored;
    logic tag_hit;

    assign tag_valid  = tag_rd[20];
    assign tag_stored = tag_rd[19:0];
    assign tag_hit    = bram_addr_valid && tag_valid && (tag_stored == bram_tag);

    // Does the BRAM output match what the CPU wants?
    logic addr_match;
    assign addr_match = (cpu_addr == bram_addr_reg);

    // ========================================================================
    // Word selection from cache line
    // ========================================================================
    logic [31:0] line_word;
    always_comb begin
        case (bram_word)
            2'd0: line_word = data_rd[31:0];
            2'd1: line_word = data_rd[63:32];
            2'd2: line_word = data_rd[95:64];
            2'd3: line_word = data_rd[127:96];
        endcase
    end

    // ========================================================================
    // FSM
    // ========================================================================
    typedef enum logic [2:0] {
        S_IDLE      = 3'd0,
        S_REFILL_0  = 3'd1,
        S_REFILL_1  = 3'd2,
        S_REFILL_2  = 3'd3,
        S_REFILL_3  = 3'd4,
        S_REFILL_DONE = 3'd5,
        S_BRAM_REREAD = 3'd6    // Wait for BRAM re-read after refill
    } ic_state_t;

    ic_state_t state;

    logic [31:0] refill_buf [0:3];
    logic [31:0] refill_addr;

    // ========================================================================
    // Hit detection (combinational)
    // ========================================================================
    logic delivering_hit;
    assign delivering_hit = (state == S_IDLE) && bram_addr_valid && addr_match && tag_hit;

    // ========================================================================
    // BRAM address mux (combinational)
    // ========================================================================
    // When delivering a hit: speculatively read BRAM for cpu_addr + 4.
    // When not delivering: read BRAM for cpu_addr (the address CPU wants).
    // After refill BRAM write: re-read at refill address.
    logic [31:0] bram_rd_addr;

    always_comb begin
        if (state == S_REFILL_DONE || state == S_BRAM_REREAD)
            bram_rd_addr = refill_addr;      // Re-read after refill write (highest priority)
        else if (redirect)
            bram_rd_addr = redirect_addr;    // Redirect: start reading target immediately
        else if (delivering_hit)
            bram_rd_addr = cpu_addr + 32'd4; // Speculative next-sequential
        else
            bram_rd_addr = cpu_addr;         // Read what CPU wants
    end

    // ========================================================================
    // Output logic (combinational)
    // ========================================================================
    always_comb begin
        cpu_rdata = 32'd0;
        cpu_ready = 1'b0;
        mem_req   = 1'b0;
        mem_addr  = 32'd0;

        case (state)
            S_IDLE: begin
                if (delivering_hit) begin
                    cpu_rdata = line_word;
                    cpu_ready = 1'b1;
                end
                // else: not ready (BRAM output mismatch or miss)
            end

            S_REFILL_0: begin
                mem_req  = 1'b1;
                mem_addr = {refill_addr[31:4], 4'b0000};
            end
            S_REFILL_1: begin
                mem_req  = 1'b1;
                mem_addr = {refill_addr[31:4], 4'b0100};
            end
            S_REFILL_2: begin
                mem_req  = 1'b1;
                mem_addr = {refill_addr[31:4], 4'b1000};
            end
            S_REFILL_3: begin
                mem_req  = 1'b1;
                mem_addr = {refill_addr[31:4], 4'b1100};
            end

            S_REFILL_DONE: ;  // BRAM write cycle, not ready
            S_BRAM_REREAD: ;  // BRAM re-read cycle, not ready

            default: ;
        endcase
    end

    // ========================================================================
    // BRAM read (synchronous, address from combinational mux)
    // ========================================================================
    always_ff @(posedge clk) begin
        tag_rd  <= tag_mem[bram_rd_addr[11:4]];
        data_rd <= data_mem[bram_rd_addr[11:4]];
    end

    // ========================================================================
    // Main FSM + BRAM tracking
    // ========================================================================
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state           <= S_IDLE;
            bram_addr_reg   <= 32'd0;
            bram_addr_valid <= 1'b0;
            refill_addr     <= 32'd0;
            // Invalidate all tags
            for (int i = 0; i < 256; i++)
                tag_mem[i] = 21'd0;
        end else begin
            // Track what BRAM is reading this cycle (will be output next cycle)
            bram_addr_reg   <= bram_rd_addr;
            bram_addr_valid <= 1'b1;

            case (state)
                S_IDLE: begin
                    // Redirect has priority over miss-start. If both happen in the
                    // same cycle, don't start a refill for the old path address.
                    if (!redirect && bram_addr_valid && addr_match && !tag_hit) begin
                        // Cache miss — start refill
                        state       <= S_REFILL_0;
                        refill_addr <= cpu_addr;
                    end
                    // On addr_match=0 (redirect): stay in IDLE.
                    // bram_rd_addr = cpu_addr → next cycle BRAM output will match.
                    // On hit: stay in IDLE. bram_rd_addr = cpu_addr+4 (speculative).
                end

                S_REFILL_0: begin
                    if (mem_ready) begin
                        refill_buf[0] <= mem_rdata;
                        state <= S_REFILL_1;
                    end
                end

                S_REFILL_1: begin
                    if (mem_ready) begin
                        refill_buf[1] <= mem_rdata;
                        state <= S_REFILL_2;
                    end
                end

                S_REFILL_2: begin
                    if (mem_ready) begin
                        refill_buf[2] <= mem_rdata;
                        state <= S_REFILL_3;
                    end
                end

                S_REFILL_3: begin
                    if (mem_ready) begin
                        refill_buf[3] <= mem_rdata;
                        state <= S_REFILL_DONE;
                    end
                end

                S_REFILL_DONE: begin
                    // Write tag and data BRAMs
                    tag_mem[refill_addr[11:4]]  <= {1'b1, refill_addr[31:12]};
                    data_mem[refill_addr[11:4]] <= {refill_buf[3], refill_buf[2],
                                                    refill_buf[1], refill_buf[0]};
                    // Need 1 cycle for BRAM re-read (write takes effect, then read)
                    state <= S_BRAM_REREAD;
                end

                S_BRAM_REREAD: begin
                    // BRAM was re-read at refill_addr last cycle.
                    // This cycle, BRAM is being read again (bram_rd_addr = refill_addr).
                    // Next cycle, output will be valid.
                    state <= S_IDLE;
                end

                default: state <= S_IDLE;
            endcase
        end
    end

endmodule
