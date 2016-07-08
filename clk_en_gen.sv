module clk_en_gen #(
    parameter INPUT_FREQ  = 128,
    parameter INPUT_WIDTH = 8,
    parameter WIDTH = 8
) (
    input  wire clk,
    input  wire rst,
    input  wire [INPUT_WIDTH - 1:0] freq,
    output reg  en
);

logic [WIDTH - 1:0] counter, counter_nxt;

assign counter_nxt = counter + freq;

always_ff @(posedge clk or posedge rst) begin
    if (rst) begin
        counter <= 0;
        en <= 1;
    end
    else if (counter_nxt >= INPUT_FREQ) begin
        counter <= counter_nxt - INPUT_FREQ;
        en <= 1;
    end
    else begin
        counter <= counter_nxt;
        en <= 0;
    end
end

endmodule
