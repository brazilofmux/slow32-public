// slow32_divider.sv — 32-cycle unsigned restoring divider
//
// Takes unsigned operands, produces unsigned quotient and remainder.
// Signed handling (abs + sign fixup) is done in the CPU.
// Algorithm: shift-subtract-restore, one quotient bit per cycle.
// Critical path: a single 33-bit subtractor.

module slow32_divider (
    input  logic        clk,
    input  logic        rst_n,

    input  logic        start,         // Pulse to begin division
    input  logic [31:0] dividend,
    input  logic [31:0] divisor,

    output logic        done,          // Pulses for one cycle when complete
    output logic [31:0] quotient,
    output logic [31:0] remainder
);

    logic [32:0] accum;     // 33-bit remainder accumulator
    logic [31:0] quo;       // Quotient being built
    logic [31:0] div_reg;   // Latched divisor
    logic [31:0] dvd_reg;   // Latched dividend (shifted out MSB-first)
    logic [4:0]  count;     // Bit counter (31 downto 0)
    logic        busy;

    // Trial subtraction
    wire [32:0] shifted = {accum[31:0], dvd_reg[31]};
    wire [32:0] trial   = shifted - {1'b0, div_reg};

    assign quotient  = quo;
    assign remainder = accum[31:0];

    always_ff @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            accum   <= 33'd0;
            quo     <= 32'd0;
            div_reg <= 32'd0;
            dvd_reg <= 32'd0;
            count   <= 5'd0;
            busy    <= 1'b0;
            done    <= 1'b0;
        end else begin
            done <= 1'b0;

            if (start && !busy) begin
                accum   <= 33'd0;
                quo     <= 32'd0;
                div_reg <= divisor;
                dvd_reg <= dividend;
                count   <= 5'd31;
                busy    <= 1'b1;
            end else if (busy) begin
                // Shift-subtract-restore iteration
                if (trial[32] == 1'b0) begin
                    // trial >= 0: keep subtraction, set quotient bit
                    accum <= trial;
                    quo   <= {quo[30:0], 1'b1};
                end else begin
                    // trial < 0: restore (use shifted, not trial)
                    accum <= shifted;
                    quo   <= {quo[30:0], 1'b0};
                end

                // Shift dividend for next iteration
                dvd_reg <= {dvd_reg[30:0], 1'b0};

                if (count == 5'd0) begin
                    busy <= 1'b0;
                    done <= 1'b1;
                end else begin
                    count <= count - 5'd1;
                end
            end
        end
    end

endmodule
