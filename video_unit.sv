module video_unit (
    /*  VGA IOs */
    input  wire clk,
    // Reset, active high
    input  wire rst,
    output wire [7:0] red,
    output wire [7:0] green,
    output wire [7:0] blue,
    output reg hsync,
    output reg vsync,

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
logic [31:0] color;
logic [14:0] addr;

assign addr = {1'd0, y[7:1], x[7:1]} + cr_base;

/* Mux to control if it's a control register access or memory access */
logic is_mem_access;
logic [31:0] bram_read;
reg   [31:0] cr_read;

assign is_mem_access = mem_addr[15] == 0;
assign mem_read = is_mem_access ? bram_read : cr_read;

/* Control registers */
reg   [14:0] cr_base;
reg   [14:0] cr_base_delay;

always_ff @(posedge mem_clk)
    if (mem_en & !is_mem_access) begin
        case (mem_addr[14:0])
            15'd0: begin
                cr_read <= {17'd0, cr_base};
                if (&mem_we) cr_base_delay <= mem_write[14:0];
            end
            default:
                cr_read <= 32'd0;
        endcase
    end

// Delay write to cr_base until next vsync to avoid tearing
always_ff @(posedge clk) begin
    if (vsync == 0)
        cr_base <= cr_base_delay;
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
    .read_b  (color)
);

vga_controller vga(
    .*,
    .color (color[23:0])
);

endmodule
