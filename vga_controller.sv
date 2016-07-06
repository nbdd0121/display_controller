module vga_controller # (
    parameter H_SYNC_ACTIVE = 0,
    parameter V_SYNC_ACTIVE = 0,

    parameter H_FRONT_PORCH = 16'd16,
    parameter H_SYNC_PULSE  = 16'd96,
    parameter H_FRAME_WIDTH = 16'd640,
    parameter H_BACK_PORCH  = 16'd48,
    parameter H_TOTAL_WIDTH = 16'd800,

    parameter V_FRONT_PORCH = 16'd10,
    parameter V_SYNC_PULSE  = 16'd2,
    parameter V_FRAME_WIDTH = 16'd480,
    parameter V_BACK_PORCH  = 16'd33,
    parameter V_TOTAL_WIDTH = 16'd525
) (
    input  wire clk,
    // Reset, active high
    input  wire rst,
    output wire [7:0] red,
    output wire [7:0] green,
    output wire [7:0] blue,
    output reg  hsync,
    output reg  vsync,

    // External image provider
    output wire en,
    output wire [15:0] x,
    output wire [15:0] y,
    input  wire [23:0] color
);

reg [15:0] h_counter;
reg [15:0] v_counter;
reg hsync_delay, vsync_delay;
reg en_delayed;

// Logic to update h and v counters
always_ff @(posedge clk or posedge rst)
begin
    if (rst)
        begin
            h_counter <= 0;
            v_counter <= 0;
        end
    else
        begin
            if (h_counter == H_TOTAL_WIDTH - 1)
                begin
                    h_counter <= 0;
                    if (v_counter == V_TOTAL_WIDTH - 1)
                        v_counter <= 0;
                    else
                        v_counter <= v_counter + 1;
                end
            else
                begin
                    h_counter <= h_counter + 1;
                end
        end
end

// This delays the generation of hsync and vsync signals by one clock cycle
// since we need one clock cycle to get the RGB data
always_ff @(posedge clk)
begin
    hsync <= hsync_delay;
    if (h_counter >= H_FRAME_WIDTH + H_FRONT_PORCH && h_counter < H_FRAME_WIDTH + H_FRONT_PORCH + H_SYNC_PULSE)
        hsync_delay <= H_SYNC_ACTIVE;
    else
        hsync_delay <= !H_SYNC_ACTIVE;
end

always_ff @(posedge clk)
begin
    vsync <= vsync_delay;
    if (v_counter >= V_FRAME_WIDTH + V_FRONT_PORCH && v_counter < V_FRAME_WIDTH + V_FRONT_PORCH + V_SYNC_PULSE)
        vsync_delay <= V_SYNC_ACTIVE;
    else
        vsync_delay <= !V_SYNC_ACTIVE;
end

always_ff @(posedge clk) begin
    en_delayed <= en;
end

// Wire to image provider
// `en` check is not necessary as we've disabled clock when `en` is false
// but this is just an additional safe guard
assign x = en ? h_counter : 0;
assign y = en ? v_counter : 0;

// Enable image output
assign en = h_counter < H_FRAME_WIDTH && v_counter < V_FRAME_WIDTH;

// Output color if enabled
assign red   = en_delayed ? color[23:16] : 0;
assign green = en_delayed ? color[15: 8] : 0;
assign blue  = en_delayed ? color[ 7: 0] : 0;

endmodule
