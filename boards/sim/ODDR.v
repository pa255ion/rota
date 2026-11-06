// Simulation-only (Verilator) stand-in for the Xilinx 7-series ODDR primitive, which
// Clash's Clash.Xilinx.DDR.oddr instantiates (e.g. the HDMI pixel clock in
// Platform.Board.Zedboard). Hand-written because the real primitive only
// exists inside Vivado; Verilator finds it through -y boards/sim. Models
// what our designs use: DDR_CLK_EDGE "SAME_EDGE" (D1 and D2 both sampled
// on the rising edge; Q shows D1 for the first half of the cycle and D2
// for the second), CE, and R (synchronous or asynchronous, both treated
// as synchronous here). S is ignored.
`timescale 100fs/100fs  // as Clash's output, so Verilator sees one timescale
/* verilator lint_off UNUSEDPARAM */
/* verilator lint_off UNUSEDSIGNAL */
module ODDR #(
    parameter DDR_CLK_EDGE = "SAME_EDGE",
    parameter INIT = 1'b0,
    parameter SRTYPE = "SYNC"
) (
    output wire Q,
    input  wire C,
    input  wire CE,
    input  wire D1,
    input  wire D2,
    input  wire R,
    input  wire S
);
    reg q1 = INIT;
    reg q2 = INIT;
    always @(posedge C) begin
        if (R) begin
            q1 <= INIT;
            q2 <= INIT;
        end else if (CE) begin
            q1 <= D1;
            q2 <= D2;
        end
    end
    assign Q = C ? q1 : q2;
endmodule
/* verilator lint_on UNUSEDSIGNAL */
/* verilator lint_on UNUSEDPARAM */
