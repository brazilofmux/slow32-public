// slow32_dcache.sv — 4KB direct-mapped data cache with write buffer
//
// 256 lines × 16 bytes/line = 4KB. Direct-mapped, write-through/write-around.
// BRAM-based: 1-cycle read latency for loads.
//
// Address decomposition (32-bit):
//   [31:12] tag (20 bits) | [11:4] index (8 bits) | [3:2] word (2 bits) | [1:0] byte
//
// Write policy:
//   - Stores go to 1-entry write buffer (0 cycles if empty)
//   - Cache line at that index is invalidated (coarse, no tag check)
//   - Write buffer drains before any load BRAM read (coherency)
//
// "done" flag prevents re-processing of the same instruction when the
// pipeline is stalled by external factors (I-cache miss, divider).
// Cleared when the request changes (new instruction enters MEM stage).

module slow32_dcache (
    input  logic        clk,
    input  logic        rst_n,

    // CPU side
    input  logic [31:0] cpu_addr,
    input  logic [31:0] cpu_wdata,
    input  logic [3:0]  cpu_byte_en,
    input  logic        cpu_wen,
    input  logic        cpu_ren,
    output logic [31:0] cpu_rdata,
    output logic        cpu_ready,

    // Memory bus side
    output logic        mem_req,
    output logic        mem_wr,
    output logic [31:0] mem_addr,
    output logic [31:0] mem_wdata,
    output logic [3:0]  mem_byte_en,
    input  logic        mem_ready,
    input  logic [31:0] mem_rdata
);

    // ========================================================================
    // BRAM arrays
    // ========================================================================
    logic [20:0] tag_mem [0:255];
    logic [20:0] tag_rd;
    logic [127:0] data_mem [0:255];
    logic [127:0] data_rd;

    // ========================================================================
    // Registered request (for BRAM pipeline)
    // ========================================================================
    logic [31:0] req_addr;

    logic [19:0] req_tag;
    logic [1:0]  req_word;

    assign req_tag  = req_addr[31:12];
    assign req_word = req_addr[3:2];

    // ========================================================================
    // Tag comparison
    // ========================================================================
    logic tag_valid, tag_hit;
    logic [19:0] tag_stored;

    assign tag_valid  = tag_rd[20];
    assign tag_stored = tag_rd[19:0];
    assign tag_hit    = tag_valid && (tag_stored == req_tag);

    // ========================================================================
    // Word selection
    // ========================================================================
    logic [31:0] line_word;
    always_comb begin
        case (req_word)
            2'd0: line_word = data_rd[31:0];
            2'd1: line_word = data_rd[63:32];
            2'd2: line_word = data_rd[95:64];
            2'd3: line_word = data_rd[127:96];
        endcase
    end

    // ========================================================================
    // Write buffer (1 entry)
    // ========================================================================
    logic        wb_valid;
    logic [31:0] wb_addr;
    logic [31:0] wb_data;
    logic [3:0]  wb_byte_en;

    // ========================================================================
    // "Done" flag — tracks completion of the CURRENT instruction in MEM.
    // Cleared when the request changes (new instruction detected).
    // ========================================================================
    logic        done;
    logic [31:0] done_rdata;    // Cached load result

    // Previous-cycle request tracking (to detect new instructions)
    logic        prev_ren;
    logic        prev_wen;
    logic [31:0] prev_addr;

    // New request = any change in ren/wen/addr means a different instruction
    logic new_request;
    assign new_request = (cpu_ren != prev_ren) || (cpu_wen != prev_wen) ||
                         (cpu_addr != prev_addr);

    // Effective done: suppressed when new_request detected (stale done from
    // previous instruction should not affect the new one)
    logic eff_done;
    assign eff_done = done && !new_request;

    // ========================================================================
    // FSM
    // ========================================================================
    typedef enum logic [2:0] {
        S_IDLE        = 3'd0,
        S_WB_DRAIN    = 3'd1,   // Draining write buffer before load
        S_BRAM_WAIT   = 3'd2,   // BRAM output pending
        S_REFILL_0    = 3'd3,
        S_REFILL_1    = 3'd4,
        S_REFILL_2    = 3'd5,
        S_REFILL_3    = 3'd6,
        S_REFILL_DONE = 3'd7    // Write BRAM + need re-read
    } dc_state_t;

    dc_state_t state;

    logic [31:0] refill_buf [0:3];
    logic [31:0] refill_addr;
    logic        reread_pending;

    // ========================================================================
    // Output logic
    // ========================================================================
    always_comb begin
        cpu_rdata   = 32'd0;
        cpu_ready   = 1'b0;
        mem_req     = 1'b0;
        mem_wr      = 1'b0;
        mem_addr    = 32'd0;
        mem_wdata   = 32'd0;
        mem_byte_en = 4'b0000;

        // "Done" overrides: operation already completed for this instruction.
        if (eff_done && (cpu_ren || cpu_wen)) begin
            cpu_ready = 1'b1;
            cpu_rdata = done_rdata;
        end else begin
            case (state)
                S_IDLE: begin
                    if (cpu_wen) begin
                        // Store: accept immediately if buffer empty
                        cpu_ready = !wb_valid;
                    end else if (cpu_ren) begin
                        cpu_ready = 1'b0;  // Load starts BRAM pipeline
                    end else begin
                        cpu_ready = 1'b1;
                    end
                end

                S_WB_DRAIN: begin
                    mem_req     = 1'b1;
                    mem_wr      = 1'b1;
                    mem_addr    = wb_addr;
                    mem_wdata   = wb_data;
                    mem_byte_en = wb_byte_en;
                end

                S_BRAM_WAIT: begin
                    if (reread_pending) begin
                        cpu_ready = 1'b0;
                    end else if (tag_hit) begin
                        cpu_rdata = line_word;
                        cpu_ready = 1'b1;
                    end
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

                S_REFILL_DONE: ;

                default: ;
            endcase
        end

        // Write buffer drain in background (S_IDLE, when no load pending)
        if (state == S_IDLE && wb_valid && !cpu_ren && !eff_done) begin
            mem_req     = 1'b1;
            mem_wr      = 1'b1;
            mem_addr    = wb_addr;
            mem_wdata   = wb_data;
            mem_byte_en = wb_byte_en;
        end
    end

    // ========================================================================
    // BRAM read
    // ========================================================================
    always_ff @(posedge clk) begin
        if (state == S_REFILL_DONE) begin
            tag_rd  <= tag_mem[refill_addr[11:4]];
            data_rd <= data_mem[refill_addr[11:4]];
        end else begin
            tag_rd  <= tag_mem[cpu_addr[11:4]];
            data_rd <= data_mem[cpu_addr[11:4]];
        end
    end

    // ========================================================================
    // Main FSM
    // ========================================================================
    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            state          <= S_IDLE;
            wb_valid       <= 1'b0;
            req_addr       <= 32'd0;
            refill_addr    <= 32'd0;
            reread_pending <= 1'b0;
            done           <= 1'b0;
            done_rdata     <= 32'd0;
            prev_ren       <= 1'b0;
            prev_wen       <= 1'b0;
            prev_addr      <= 32'd0;
            for (int i = 0; i < 256; i++)
                tag_mem[i] = 21'd0;
        end else begin
            // Track previous request for new-instruction detection
            prev_ren  <= cpu_ren;
            prev_wen  <= cpu_wen;
            prev_addr <= cpu_addr;

            // Clear "done" when request changes (new instruction in MEM)
            // or when no load/store is active
            if (new_request || (!cpu_ren && !cpu_wen))
                done <= 1'b0;

            case (state)
                S_IDLE: begin
                    // Accept store into write buffer (once per instruction)
                    if (cpu_wen && !wb_valid && !eff_done) begin
                        wb_valid   <= 1'b1;
                        wb_addr    <= cpu_addr;
                        wb_data    <= cpu_wdata;
                        wb_byte_en <= cpu_byte_en;
                        // Invalidate cache line
                        tag_mem[cpu_addr[11:4]] <= 21'd0;
                        // Mark done
                        done <= 1'b1;
                    end

                    // Start load (once per instruction)
                    if (cpu_ren && !eff_done) begin
                        req_addr <= cpu_addr;
                        if (wb_valid) begin
                            state <= S_WB_DRAIN;
                        end else begin
                            state <= S_BRAM_WAIT;
                        end
                    end

                    // Background write buffer drain
                    if (wb_valid && !cpu_ren && !eff_done && mem_ready) begin
                        wb_valid <= 1'b0;
                    end
                end

                S_WB_DRAIN: begin
                    if (mem_ready) begin
                        wb_valid <= 1'b0;
                        state    <= S_BRAM_WAIT;
                    end
                end

                S_BRAM_WAIT: begin
                    if (reread_pending) begin
                        reread_pending <= 1'b0;
                    end else if (tag_hit) begin
                        // Hit: mark done, buffer result
                        done       <= 1'b1;
                        done_rdata <= line_word;
                        state      <= S_IDLE;
                    end else begin
                        // Miss: refill
                        refill_addr <= req_addr;
                        state       <= S_REFILL_0;
                    end
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
                    tag_mem[refill_addr[11:4]]  <= {1'b1, refill_addr[31:12]};
                    data_mem[refill_addr[11:4]] <= {refill_buf[3], refill_buf[2],
                                                    refill_buf[1], refill_buf[0]};
                    reread_pending <= 1'b1;
                    state <= S_BRAM_WAIT;
                end

                default: state <= S_IDLE;
            endcase
        end
    end

endmodule
