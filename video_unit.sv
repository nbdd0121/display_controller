module video_unit (
    /*  VGA IOs */
    input  wire clk,
    // Reset, active high
    input  wire rst,
    output wire [7:0] red,
    output wire [7:0] green,
    output wire [7:0] blue,
    output reg  hsync,
    output reg  vsync,

    /* Video Memory & Control Register Acccess */
    input  wire        mem_clk,
    input  wire        mem_en,
    input  wire [3:0]  mem_we,
    input  wire [15:0] mem_addr,
    input  wire [31:0] mem_write,
    output wire [31:0] mem_read
);

/* VGA controller related logic */
logic en;
logic [15:0] x ,y;
logic [31:0] rawcolor;
logic [23:0] color;
logic [14:0] scraddr;
logic [14:0] scraddr_delayed;
logic [14:0] pxladdr;
logic [14:0] addr;

/* Mux to control if it's a control register access or memory access */
logic is_mem_access;
logic [31:0] bram_read;
reg   [31:0] cr_read;

assign is_mem_access = mem_addr[15] == 0;
assign mem_read = is_mem_access ? bram_read : cr_read;

/* Control registers */
reg   [14:0] cr_base;
reg   [14:0] cr_base_delay;

reg   [1:0]  cr_depth;
reg   [1:0]  cr_depth_delay;

localparam CR_BASE  = 15'd0;
localparam CR_DEPTH = 15'd1;

/* Address calculation */
always_comb begin
    scraddr = {1'd0, y[7:1], x[7:1]};
    case (cr_depth)
        2'b00:
            pxladdr = scraddr;
        2'b01:
            pxladdr = {1'b0, scraddr[14:1]};
        2'b10:
            pxladdr = {1'b00, scraddr[14:2]};
        2'b11:
            pxladdr = {1'b000, scraddr[14:3]};
    endcase
    addr = pxladdr + cr_base;
end

/* Color extraction */

function [23:0] unpack16 (input [15:0] color);
    unpack16 = {
        color[15:11], color[15:13],
        color[10: 5], color[10: 9],
        color[ 4: 0], color[ 4: 2]
    };
endfunction

function [23:0] unpack8 (input [7:0] color);
    unpack8 = {
        {2{color[7:5]}}, color[7:6],
        {2{color[4:2]}}, color[4:3],
        {4{color[1:0]}}
    };
endfunction

always_ff @(posedge clk)
    scraddr_delayed <= scraddr;

always_comb begin
    case (cr_depth)
        2'b00:
            color = rawcolor[23:0];
        2'b01:
            color = unpack16(scraddr_delayed[0] ? rawcolor[31:16] : rawcolor[15:0]);
        2'b10:
            case (scraddr_delayed[1:0])
                2'b00: color = unpack8(rawcolor[ 7: 0]);
                2'b01: color = unpack8(rawcolor[15: 8]);
                2'b10: color = unpack8(rawcolor[23:16]);
                2'b11: color = unpack8(rawcolor[31:24]);
            endcase
        2'b11:
            case (scraddr_delayed[2:0])
                3'b000: color = {6{rawcolor[ 3: 0]}};
                3'b001: color = {6{rawcolor[ 7: 4]}};
                3'b010: color = {6{rawcolor[11: 8]}};
                3'b011: color = {6{rawcolor[15:12]}};
                3'b100: color = {6{rawcolor[19:16]}};
                3'b101: color = {6{rawcolor[23:20]}};
                3'b110: color = {6{rawcolor[27:24]}};
                3'b111: color = {6{rawcolor[31:28]}};
            endcase
    endcase
end

/* Control register R/W */
always_ff @(posedge mem_clk or posedge rst)
    if (rst) begin
        cr_base_delay <= 15'd0;
        cr_depth_delay <= 2'd0;
    end
    else if (mem_en & !is_mem_access) begin
        case (mem_addr[14:0])
            CR_BASE: begin
                cr_read <= {17'd0, cr_base};
                if (&mem_we) cr_base_delay <= mem_write[14:0];
            end
            CR_DEPTH: begin
                cr_read <= {30'd0, cr_depth};
                if (&mem_we) cr_depth_delay <= mem_write[1:0];
            end
            default:
                cr_read <= 32'd0;
        endcase
    end

// Delay write to cr_base until next vsync to avoid tearing
always_ff @(posedge clk)
    if (vsync == 0) begin
        cr_base  <= cr_base_delay;
        cr_depth <= cr_depth_delay;
    end


dual_port_bram #(
    .ADDR_WIDTH (15)
) videomem (
    .clk_a   (mem_clk),
    .en_a    (mem_en & is_mem_access),
    .we_a    (mem_we),
    .addr_a  (mem_addr[14:0]),
    .write_a (mem_write),
    .read_a  (bram_read),

    .clk_b   (clk),
    .en_b    (en),
    .we_b    (4'd0),
    .addr_b  (addr),
    .write_b (32'd0),
    .read_b  (rawcolor)
);

vga_controller vga(
    .*
);

endmodule
