`include "axi_util.svh"

module video_unit (
  // Bus clock and reset.
  input  logic clk_i,
  input  logic rst_ni,

  // Pixel clock control and input
  output logic [7:0] pxl_clk_freq_o,
  input  logic       pxl_clk_i,
  input  logic       pxl_clk_en_i,

   // Video output signals.
   // They are synchronized to the pixel clock.
  output [7:0] red_o,
  output [7:0] green_o,
  output [7:0] blue_o,
  output logic pixel_o,
  output logic hsync_o,
  output logic vsync_o,

  // BRAM port for control registers, 256 Bytes in size
  input  logic        ctrl_en_i,
  input  logic        ctrl_we_i,
  input  logic [7:0]  ctrl_addr_i,
  input  logic [31:0] ctrl_wrdata_i,
  output logic [31:0] ctrl_rddata_o,

  `AXI_DECLARE_HOST_PORT(DataWidth, AddrWidth, IdWidth, dma)
);

  //////////////////////////////
  // region Control registers //

  // Register addresses
  localparam CR_ENABLE     = 6'h00;
  localparam CR_PXLFREQ    = 6'h01;
  localparam CR_POLARITY   = 6'h02;
  localparam CR_H_TOTAL    = 6'h08;
  localparam CR_H_END_DISP = 6'h09;
  localparam CR_H_SRT_SYNC = 6'h0A;
  localparam CR_H_END_SYNC = 6'h0B;
  localparam CR_V_TOTAL    = 6'h0C;
  localparam CR_V_END_DISP = 6'h0D;
  localparam CR_V_SRT_SYNC = 6'h0E;
  localparam CR_V_END_SYNC = 6'h0F;

  localparam CR_FB_ENABLE    = 6'h10;
  localparam CR_FB_BASE      = 6'h12;
  localparam CR_FB_BASE_HIGH = 6'h13;
  localparam CR_FB_WIDTH     = 6'h14;
  localparam CR_FB_HEIGHT    = 6'h15;
  localparam CR_FB_DEPTH     = 6'h16;
  localparam CR_FB_BPL       = 6'h17;
  localparam CR_BG_COLOR     = 6'h18;

  // Whether display output is enabled.
  logic cr_enable;

  // Pixel frequency. Can only be changed when display is disabled.
  logic [7:0] cr_pxlfreq;
  assign pxl_clk_freq_o = cr_pxlfreq;

  // HSync, VSync polarity. 0 selects positive, 1 selects negative.
  // Can only be changed when display is disabled.
  logic cr_hsync_pol;
  logic cr_vsync_pol;

  // Timing registers. Can only be changed when display is disabled.
  logic [15:0] cr_h_total;
  logic [15:0] cr_h_end_disp;
  logic [15:0] cr_h_srt_sync;
  logic [15:0] cr_h_end_sync;
  logic [15:0] cr_v_total;
  logic [15:0] cr_v_end_disp;
  logic [15:0] cr_v_srt_sync;
  logic [15:0] cr_v_end_sync;

  logic cr_fb_enable;

  // Base register, must be 8-byte aligned
  logic [63:0] cr_fb_base;
  logic [63:0] cr_fb_base_delay;

  // Framebuffer geometry control
  logic [11:0] cr_fb_width;
  logic [11:0] cr_fb_height;
  logic        cr_fb_depth;
  // Since this is fed to DMA, it must be 8-byte aligned
  logic [13:0] cr_fb_bpl;

  // Fallback color for pixcels outside framebuffer
  logic [23:0] cr_bg_color;
  logic [23:0] cr_bg_color_delay;

  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      cr_enable      <= 1'd0;
      cr_pxlfreq     <= 8'd25;
      cr_hsync_pol   <= 1'd1;
      cr_vsync_pol   <= 1'd1;

      cr_h_total     <= 16'd800;
      cr_h_end_disp  <= 16'd640;
      cr_h_srt_sync  <= 16'd656;
      cr_h_end_sync  <= 16'd752;
      cr_v_total     <= 16'd525;
      cr_v_end_disp  <= 16'd480;
      cr_v_srt_sync  <= 16'd490;
      cr_v_end_sync  <= 16'd492;

      cr_fb_enable      <= 1'b0;
      cr_fb_base_delay  <= 15'd0;
      cr_fb_width       <= 12'd640;
      cr_fb_height      <= 12'd480;
      cr_fb_depth       <= 1'd0;
      cr_fb_bpl         <= 14'd2048;
      cr_bg_color_delay <= 24'hFFFFFF;
    end else begin
      if (ctrl_en_i) begin
        unique case (ctrl_addr_i[7:2])
          CR_ENABLE    : ctrl_rddata_o <= {31'd0, cr_enable};
          CR_PXLFREQ   : ctrl_rddata_o <= {24'd0, cr_pxlfreq};
          CR_POLARITY  : ctrl_rddata_o <= {30'd0, cr_vsync_pol, cr_hsync_pol};

          CR_H_TOTAL   : ctrl_rddata_o <= {16'd0, cr_h_total   };
          CR_H_END_DISP: ctrl_rddata_o <= {16'd0, cr_h_end_disp};
          CR_H_SRT_SYNC: ctrl_rddata_o <= {16'd0, cr_h_srt_sync};
          CR_H_END_SYNC: ctrl_rddata_o <= {16'd0, cr_h_end_sync};
          CR_V_TOTAL   : ctrl_rddata_o <= {16'd0, cr_v_total   };
          CR_V_END_DISP: ctrl_rddata_o <= {16'd0, cr_v_end_disp};
          CR_V_SRT_SYNC: ctrl_rddata_o <= {16'd0, cr_v_srt_sync};
          CR_V_END_SYNC: ctrl_rddata_o <= {16'd0, cr_v_end_sync};

          CR_FB_ENABLE    : ctrl_rddata_o <= {31'd0, cr_fb_enable};
          CR_FB_BASE      : ctrl_rddata_o <= cr_fb_base[31:0];
          CR_FB_BASE_HIGH : ctrl_rddata_o <= cr_fb_base[63:32];
          CR_FB_WIDTH     : ctrl_rddata_o <= {20'd0, cr_fb_width};
          CR_FB_HEIGHT    : ctrl_rddata_o <= {20'd0, cr_fb_height};
          CR_FB_DEPTH     : ctrl_rddata_o <= {31'd0, cr_fb_depth};
          CR_FB_BPL       : ctrl_rddata_o <= {18'd0, cr_fb_bpl};
          CR_BG_COLOR     : ctrl_rddata_o <= { 8'd0, cr_bg_color};
          default: ctrl_rddata_o <= 32'd0;
        endcase

        if (&ctrl_we_i) begin
          unique case (ctrl_addr_i[7:2])
            CR_ENABLE    : cr_enable <= ctrl_wrdata_i[0];
            CR_PXLFREQ   : if (!cr_enable) cr_pxlfreq    <= ctrl_wrdata_i[ 7:0];
            CR_POLARITY  : begin
              if (!cr_enable) begin
                  cr_vsync_pol <= ctrl_wrdata_i[1];
                  cr_hsync_pol <= ctrl_wrdata_i[0];
              end
            end

            CR_H_TOTAL   : if (!cr_enable) cr_h_total    <= ctrl_wrdata_i[15:0];
            CR_H_END_DISP: if (!cr_enable) cr_h_end_disp <= ctrl_wrdata_i[15:0];
            CR_H_SRT_SYNC: if (!cr_enable) cr_h_srt_sync <= ctrl_wrdata_i[15:0];
            CR_H_END_SYNC: if (!cr_enable) cr_h_end_sync <= ctrl_wrdata_i[15:0];
            CR_V_TOTAL   : if (!cr_enable) cr_v_total    <= ctrl_wrdata_i[15:0];
            CR_V_END_DISP: if (!cr_enable) cr_v_end_disp <= ctrl_wrdata_i[15:0];
            CR_V_SRT_SYNC: if (!cr_enable) cr_v_srt_sync <= ctrl_wrdata_i[15:0];
            CR_V_END_SYNC: if (!cr_enable) cr_v_end_sync <= ctrl_wrdata_i[15:0];

            CR_FB_ENABLE   : cr_fb_enable <= ctrl_wrdata_i[0];
            CR_FB_BASE     : cr_fb_base_delay[31:0] <= ctrl_wrdata_i;
            CR_FB_BASE_HIGH: cr_fb_base_delay[63:32] <= ctrl_wrdata_i;
            CR_FB_WIDTH    : if (!cr_enable) cr_fb_width  <= ctrl_wrdata_i[11:0];
            CR_FB_HEIGHT   : if (!cr_enable) cr_fb_height <= ctrl_wrdata_i[11:0];
            CR_FB_DEPTH    : if (!cr_enable) cr_fb_depth  <= ctrl_wrdata_i[0];
            CR_FB_BPL      : if (!cr_enable) cr_fb_bpl    <= {ctrl_wrdata_i[13:3], 3'd0};
            CR_BG_COLOR    : cr_bg_color_delay  <= ctrl_wrdata_i[23:0];

          endcase
        end
      end
    end
  end

  // #endregion
  //////////////////////////////

// Delay write to cr_fb_base until next vsync to avoid tearing
always_ff @(posedge pxl_clk_i or negedge rst_ni)
   if (!rst_ni) begin
      cr_fb_base      <= 15'd0;
      cr_bg_color  <= 24'hFFFFFF;
   end
   else if (pxl_clk_en_i) begin
      if (vsync_o == !cr_vsync_pol | !cr_enable) begin
         cr_fb_base     <= cr_fb_base_delay;
         cr_bg_color <= cr_bg_color_delay;
      end
   end

/* VGA controller related logic */
logic en;
logic [63:0] rawcolor;
logic [23:0] color;
// Pixel address in memory, unit is 8-byte
logic [11:0]  pxl_addr;
// Offset within the byte
logic [7:0]  pxl_offset;
logic [7:0]  pxl_offset_delayed;

/* Framebuffer */

logic buffer_en;
logic [7:0] buffer_we;
logic [11:0] buffer_addr;
logic [63:0] buffer_wrdata;

logic dma_done;
logic [63:0] dma_src_addr;
logic [63:0] dma_dest_addr;
logic [63:0] dma_length;
logic dma_en;

  logic [63:0] buffer_we_expanded;
  always_comb begin
    for (int i = 0; i < 8; i++) begin
      buffer_we_expanded[i * 8 +: 8] <= buffer_we[i] ? 8'hff : 8'h00;
    end
  end

  // 32KiB Line Buffer, (1 << 12) * 64Bits
  prim_generic_ram_simple_2p #(
    .Width (64),
    .Depth (2 ** 12),
    .DataBitsPerMask (8)
  ) videomem (
    .clk_a_i (pxl_clk_i),
    .clk_b_i (clk_i),
    .a_req_i (en & pxl_clk_en_i),
    .a_addr_i (pxl_addr),
    .a_rdata_o (rawcolor),
    .b_req_i (buffer_en),
    .b_addr_i (buffer_addr),
    .b_wdata_i (buffer_wrdata),
    .b_wmask_i (buffer_we_expanded)
  );

  `AXI_DECLARE(DataWidth, AddrWidth, IdWidth, buffer);

  // Internal NASTI BRAM Controller to write to the buffer
  axi_bram_ctrl # (
    .AddrWidth (64),
    .DataWidth (64),
    .BRAM_ADDR_WIDTH (12)
  ) buffer_ctrl (
    .clk_i (clk_i),
    .rst_ni (rst_ni),
    `AXI_CONNECT_DEVICE_PORT(host, buffer),
    .bram_en (buffer_en),
    .bram_we (buffer_we),
    .bram_addr (buffer_addr),
    .bram_wrdata (buffer_wrdata),
    .bram_rddata ()
  );

  // Data mover from framebuffer to internal buffer
  axi_data_mover # (
    .AddrWidth (64),
    .DataWidth (64),
    .MaxBurstLen (8)
  ) data_mover (
    .clk_i (clk_i),
    .rst_ni (rst_ni),
    `AXI_FORWARD_HOST_PORT(src, dma),
    `AXI_CONNECT_HOST_PORT(dst, buffer),
    .ready_o (dma_done),
    .valid_i (dma_en),
    .src_i (dma_src_addr),
    .dst_i (dma_dest_addr),
    .len_i (dma_length)
  );

/* VGA Controller */

// This is a three-stage pipeline VGA controller
// In the first stage, updated value of h_counter and v_counter
// In the second stage, screen address will be updated and DMA request for next line is initiated
// In the third stage, BRAM will be accessed to retrieve the pixel

// First stage

logic [15:0] h_counter;
logic [15:0] v_counter;

// Logic to update h and v counters
always_ff @(posedge pxl_clk_i or negedge rst_ni) begin
  if (!rst_ni) begin
    h_counter <= 0;
    v_counter <= 0;
  end else begin
    if (!cr_enable) begin
      // If monitor is turned off, reset counters
      h_counter <= 0;
      v_counter <= 0;
    end else if (pxl_clk_en_i) begin
      if (h_counter == cr_h_total - 1) begin
        h_counter <= 0;
        if (v_counter == cr_v_total - 1) begin
          v_counter <= 0;
        end else begin
          v_counter <= v_counter + 1;
        end
      end else begin
        h_counter <= h_counter + 1;
      end
    end
  end
end

// Second stage

logic hsync_delay, vsync_delay;

// These values should be delayed until next aclk
// to avoid inter-clock slacks
logic dma_done_delayed;
logic [63:0] dma_src_addr_delay;
logic [63:0] dma_dest_addr_delay;
logic [63:0] dma_length_delay;
logic dma_en_delay;

logic pxl_en;

always_ff @(posedge pxl_clk_i) begin
   if (pxl_clk_en_i) begin
      // Mute output when monitor is disabled
      if (!cr_enable)
         hsync_delay <= 0;
      else if (h_counter >= cr_h_srt_sync && h_counter < cr_h_end_sync)
         hsync_delay <= !cr_hsync_pol;
      else
         hsync_delay <= cr_hsync_pol;

      if (!cr_enable)
         vsync_delay <= 0;
      else if (v_counter >= cr_v_srt_sync && v_counter < cr_v_end_sync)
         vsync_delay <= !cr_vsync_pol;
      else
         vsync_delay <= cr_vsync_pol;

      if (!cr_enable) begin
         en <= 0;
         pxl_en <= 0;
      end
      else begin
         // Enable image output if within display area
         en <= h_counter < cr_fb_width && v_counter < cr_fb_height;
         pxl_en <= h_counter < cr_h_end_disp && v_counter < cr_v_end_disp && cr_fb_enable;
      end
   end
end

logic [14:0] fb_width_in_bytes;

always_comb begin
   case (cr_fb_depth)
      1'b0: fb_width_in_bytes = cr_fb_width * 4;
      1'b1: fb_width_in_bytes = cr_fb_width * 2;
   endcase
end

always_ff @(posedge pxl_clk_i or negedge rst_ni) begin
   if (!rst_ni) begin
      dma_en_delay <= 0;
      dma_src_addr_delay <= 0;
   end
   else if (pxl_clk_en_i && cr_enable) begin
      if (h_counter == 0) begin
         if (v_counter < cr_fb_height) begin
            pxl_addr <= {v_counter[0], 11'd0};
            pxl_offset <= 0;

            if (v_counter == 0)
               // Special case if this is first line
               // After screen is re-enabled, we need to make sure
               // dma_src_addr_delay is updated
               dma_src_addr_delay <= cr_fb_base_delay + cr_fb_bpl;
            else
            if (v_counter == cr_fb_height - 1)
               // For last line, reset start address to base
               dma_src_addr_delay <= cr_fb_base_delay;
            else
               dma_src_addr_delay <= dma_src_addr_delay + cr_fb_bpl;

            dma_dest_addr_delay <= {49'd0, ~v_counter[0], 14'd0};
            dma_length_delay <= fb_width_in_bytes;
            dma_en_delay <= 1;
         end
      end
      else begin
         // Advance screen address and offset within qword, depending on color depth
         case (cr_fb_depth)
            1'b0:
               if (pxl_offset == 32) begin
                  pxl_addr <= pxl_addr + 1;
                  pxl_offset <= 0;
               end
               else
                  pxl_offset <= pxl_offset + 32;
            1'b1:
               if (pxl_offset == 48) begin
                  pxl_addr <= pxl_addr + 1;
                  pxl_offset <= 0;
               end
               else
                  pxl_offset <= pxl_offset + 16;
         endcase

         // Prevent data mover from repeating the action
         if (dma_done_delayed == 0) dma_en_delay <= 0;
      end
   end
end

always_ff @(posedge clk_i or negedge rst_ni) begin
   if (!rst_ni) begin
      dma_en <= 0;
   end
   else begin
      dma_src_addr <= dma_src_addr_delay;
      dma_dest_addr <= dma_dest_addr_delay;
      dma_length <= dma_length_delay;
      dma_en <= dma_en_delay;
      dma_done_delayed <= dma_done;
   end
end

// Third stage

logic en_delayed;
logic pxl_en_delayed;

always_ff @(posedge pxl_clk_i) begin
   if (pxl_clk_en_i) begin
      // These delays the generation of hsync and vsync signals by one clock cycle
      // since we need one clock cycle to get the RGB data
      hsync_o <= hsync_delay;
      vsync_o <= vsync_delay;

      en_delayed <= en;
      pxl_en_delayed <= pxl_en;

      pxl_offset_delayed <= pxl_offset;
   end
end

// Combinational logic for output

function [23:0] unpack16 (input [15:0] color);
   unpack16 = {
      color[ 4: 0], color[ 4: 2],
      color[10: 5], color[10: 9],
      color[15:11], color[15:13]
   };
endfunction

always_comb begin
   case (cr_fb_depth)
      1'b0:
         color = rawcolor >> pxl_offset_delayed;
      1'b1:
         color = unpack16(rawcolor >> pxl_offset_delayed);
    endcase
end

logic [23:0] disp_color;

assign disp_color = pxl_en_delayed & cr_enable ? (en_delayed ? color : cr_bg_color) : 0;

// Output color if enabled
assign red_o   = disp_color[ 7: 0];
assign green_o = disp_color[15: 8];
assign blue_o  = disp_color[23:16];
assign pixel_o = pxl_en_delayed & cr_enable;

endmodule
