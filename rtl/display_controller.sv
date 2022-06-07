`include "axi_util.svh"

module display_controller #(
  parameter DataWidth = 64,
  parameter AddrWidth = 64,
  parameter IdWidth = 1
) (
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

  localparam DataWidthInBytes = DataWidth / 8;
  localparam NonBurstSize = $clog2(DataWidthInBytes);

  //////////////////////////////////
  // region Clock domain crossing //

  // Whether to enable display.
  // Synchronise from bus clock to pixel clock.
  logic cr_enable;
  logic cr_enable_sync;

  prim_flop_2sync #(
    .Width (1)
  ) cr_enable_cdc (
    .clk_i (pxl_clk_i),
    .rst_ni,
    .d_i (cr_enable),
    .q_o (cr_enable_sync)
  );

  // Whether display is enabled.
  // Synchronise from pixel clock to bus clock.
  logic cr_enabled;
  logic cr_enabled_sync;

  prim_flop_2sync #(
    .Width (1)
  ) cr_enabled_cdc (
    .clk_i (clk_i),
    .rst_ni,
    .d_i (cr_enabled),
    .q_o (cr_enabled_sync)
  );

  // Signals the end of a line.
  // Synchronise from pixel clock to bus clock.
  logic end_of_line;
  logic end_of_line_sync;

  prim_pulse_sync end_of_line_cdc (
    .clk_src_i (pxl_clk_i),
    .rst_src_ni (rst_ni),
    .src_pulse_i (end_of_line),
    .clk_dst_i (clk_i),
    .rst_dst_ni (rst_ni),
    .dst_pulse_o (end_of_line_sync)
  );

  // Signals the end of a frame.
  // Synchronise from pixel clock to bus clock.
  logic end_of_frame;
  logic end_of_frame_sync;

  prim_pulse_sync end_of_frame_cdc (
    .clk_src_i (pxl_clk_i),
    .rst_src_ni (rst_ni),
    .src_pulse_i (end_of_frame),
    .clk_dst_i (clk_i),
    .rst_dst_ni (rst_ni),
    .dst_pulse_o (end_of_frame_sync)
  );

  // endregion
  //////////////////////////////////

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

  localparam CR_FB_COMMIT    = 6'h10;
  localparam CR_FB_BASE      = 6'h12;
  localparam CR_FB_BASE_HIGH = 6'h13;
  localparam CR_FB_WIDTH     = 6'h14;
  localparam CR_FB_HEIGHT    = 6'h15;
  localparam CR_FB_DEPTH     = 6'h16;
  localparam CR_FB_BPL       = 6'h17;
  localparam CR_BG_COLOR     = 6'h18;

  // Pixel frequency.
  logic [7:0] cr_pxlfreq;
  assign pxl_clk_freq_o = cr_pxlfreq;

  // HSync, VSync polarity. 0 selects positive, 1 selects negative.
  logic cr_hsync_pol;
  logic cr_vsync_pol;

  // Timing registers.
  logic [15:0] cr_h_total;
  logic [15:0] cr_h_end_disp;
  logic [15:0] cr_h_srt_sync;
  logic [15:0] cr_h_end_sync;
  logic [15:0] cr_v_total;
  logic [15:0] cr_v_end_disp;
  logic [15:0] cr_v_srt_sync;
  logic [15:0] cr_v_end_sync;

  // Changes to framebuffer's properties cannot happen while a frame is being
  // rendered to avoid race condition. So we latch all these registers and only
  // update them when the display is finished.
  logic cr_fb_commit;

  // Base register, must be 8-byte aligned
  logic [63:0] cr_fb_base, cr_fb_base_latch;

  // Framebuffer geometry control
  logic [11:0] cr_fb_width, cr_fb_width_latch;
  logic [11:0] cr_fb_height, cr_fb_height_latch;
  logic        cr_fb_depth, cr_fb_depth_latch;

  // Since this is fed to DMA, it must be 8-byte aligned
  logic [13:0] cr_fb_bpl, cr_fb_bpl_latch;

  // Fallback color for pixels outside framebuffer
  logic [23:0] cr_bg_color, cr_bg_color_latch;

  // Critical timing-related registers can only be changed when the display is off.
  wire cr_allow_timing_change = !cr_enable && !cr_enabled;

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

      cr_fb_commit <= 1'b0;
      cr_fb_base   <= '0;
      cr_fb_width  <= 12'd640;
      cr_fb_height <= 12'd480;
      cr_fb_depth  <= 1'd0;
      cr_fb_bpl    <= 14'd0;
      cr_bg_color  <= 24'hFFFFFF;

      cr_fb_base_latch   <= '0;
      cr_fb_width_latch  <= 12'd640;
      cr_fb_height_latch <= 12'd480;
      cr_fb_depth_latch  <= 1'd0;
      cr_fb_bpl_latch    <= 14'd0;
      cr_bg_color_latch  <= 24'hFFFFFF;
    end else begin
      if (end_of_frame_sync && cr_fb_commit) begin
        cr_fb_commit <= 1'b0;
        cr_fb_base_latch   <= cr_fb_base;
        cr_fb_width_latch  <= cr_fb_width;
        cr_fb_height_latch <= cr_fb_height;
        cr_fb_depth_latch  <= cr_fb_depth;
        cr_fb_bpl_latch    <= cr_fb_bpl;
        cr_bg_color_latch  <= cr_bg_color;
      end

      if (ctrl_en_i) begin
        unique case (ctrl_addr_i[7:2])
          CR_ENABLE    : ctrl_rddata_o <= {31'd0, cr_enabled_sync};
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

          CR_FB_COMMIT    : ctrl_rddata_o <= {31'd0, cr_fb_commit};
          CR_FB_BASE      : ctrl_rddata_o <= cr_fb_base_latch[31:0];
          CR_FB_BASE_HIGH : ctrl_rddata_o <= cr_fb_base_latch[63:32];
          CR_FB_WIDTH     : ctrl_rddata_o <= {20'd0, cr_fb_width_latch};
          CR_FB_HEIGHT    : ctrl_rddata_o <= {20'd0, cr_fb_height_latch};
          CR_FB_DEPTH     : ctrl_rddata_o <= {31'd0, cr_fb_depth_latch};
          CR_FB_BPL       : ctrl_rddata_o <= {18'd0, cr_fb_bpl_latch};
          CR_BG_COLOR     : ctrl_rddata_o <= { 8'd0, cr_bg_color_latch};
          default: ctrl_rddata_o <= 32'd0;
        endcase

        if (&ctrl_we_i) begin
          unique case (ctrl_addr_i[7:2])
            CR_ENABLE    : cr_enable <= ctrl_wrdata_i[0];
            CR_PXLFREQ   : if (!cr_allow_timing_change) cr_pxlfreq    <= ctrl_wrdata_i[ 7:0];
            CR_POLARITY  : begin
              if (!cr_allow_timing_change) begin
                  cr_vsync_pol <= ctrl_wrdata_i[1];
                  cr_hsync_pol <= ctrl_wrdata_i[0];
              end
            end

            CR_H_TOTAL   : if (!cr_allow_timing_change) cr_h_total    <= ctrl_wrdata_i[15:0];
            CR_H_END_DISP: if (!cr_allow_timing_change) cr_h_end_disp <= ctrl_wrdata_i[15:0];
            CR_H_SRT_SYNC: if (!cr_allow_timing_change) cr_h_srt_sync <= ctrl_wrdata_i[15:0];
            CR_H_END_SYNC: if (!cr_allow_timing_change) cr_h_end_sync <= ctrl_wrdata_i[15:0];
            CR_V_TOTAL   : if (!cr_allow_timing_change) cr_v_total    <= ctrl_wrdata_i[15:0];
            CR_V_END_DISP: if (!cr_allow_timing_change) cr_v_end_disp <= ctrl_wrdata_i[15:0];
            CR_V_SRT_SYNC: if (!cr_allow_timing_change) cr_v_srt_sync <= ctrl_wrdata_i[15:0];
            CR_V_END_SYNC: if (!cr_allow_timing_change) cr_v_end_sync <= ctrl_wrdata_i[15:0];

            CR_FB_COMMIT   : cr_fb_commit      <= ctrl_wrdata_i[0];
            CR_FB_BASE     : cr_fb_base[31:0]  <= ctrl_wrdata_i;
            CR_FB_BASE_HIGH: cr_fb_base[63:32] <= ctrl_wrdata_i;
            CR_FB_WIDTH    : cr_fb_width       <= ctrl_wrdata_i[11:0];
            CR_FB_HEIGHT   : cr_fb_height      <= ctrl_wrdata_i[11:0];
            CR_FB_DEPTH    : cr_fb_depth       <= ctrl_wrdata_i[0];
            CR_FB_BPL      : cr_fb_bpl         <= {ctrl_wrdata_i[13:3], 3'd0};
            CR_BG_COLOR    : cr_bg_color       <= ctrl_wrdata_i[23:0];
          endcase
        end
      end
    end
  end

  // #endregion
  //////////////////////////////

  ////////////////////////
  // region Line buffer //

  logic        buffer_read_en;
  logic [12:0] buffer_read_addr;
  logic [31:0] buffer_read_data;

  logic [NonBurstSize-3:0]       buffer_read_addr_latch;
  logic [DataWidth/32-1:0][31:0] buffer_read_data_wide;

  always_ff @(posedge pxl_clk_i) begin
    if (pxl_clk_en_i && buffer_read_en) begin
      buffer_read_addr_latch <= buffer_read_addr[NonBurstSize-3:0];
    end
  end

  assign buffer_read_data = buffer_read_data_wide[buffer_read_addr_latch];

  logic                       buffer_write_en;
  logic [15-NonBurstSize-1:0] buffer_write_addr;
  logic [DataWidth-1:0]       buffer_write_data;
  logic [DataWidth/8-1:0]     buffer_write_strb;

  logic [DataWidth-1:0] buffer_write_strb_expanded;
  always_comb begin
    for (int i = 0; i < DataWidthInBytes; i++) begin
      buffer_write_strb_expanded[i * 8 +: 8] <= buffer_write_strb[i] ? 8'hff : 8'h00;
    end
  end

  // 32KiB Line Buffer, 4096 * 4bytes * 2
  prim_generic_ram_simple_2p #(
    .Width (DataWidth),
    .Depth (2 ** (15 - NonBurstSize)),
    .DataBitsPerMask (8)
  ) line_buffer (
    .clk_a_i (pxl_clk_i),
    .clk_b_i (clk_i),
    .a_req_i (buffer_read_en & pxl_clk_en_i),
    .a_addr_i (buffer_read_addr[12:NonBurstSize-2]),
    .a_rdata_o (buffer_read_data_wide),
    .b_req_i (buffer_write_en),
    .b_addr_i (buffer_write_addr),
    .b_wdata_i (buffer_write_data),
    .b_wmask_i (buffer_write_strb_expanded)
  );

  // endregion
  ////////////////////////

  /////////////////////////////////
  // region Video timing control //

  logic [15:0] h_counter;
  logic [15:0] v_counter;

  // Logic to update h and v counters
  always_ff @(posedge pxl_clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      h_counter <= 0;
      v_counter <= 0;
    end else begin
      if (!cr_enable_sync) begin
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

  logic hsync;
  logic vsync;
  logic video_enable;
  logic buffer_read_offset;

  logic end_of_frame_d;
  logic end_of_line_d;
  logic buffer_read_en_q;
  logic buffer_read_offset_q;

  // Video output, HSync and VSync timing.
  always_comb begin
    hsync = 1'b0;
    vsync = 1'b0;
    video_enable = 1'b0;
    end_of_line_d = 1'b0;
    end_of_frame_d = 1'b0;
    buffer_read_en = 1'b0;
    buffer_read_addr = 'x;
    buffer_read_offset = 1'b0;

    if (cr_enable_sync) begin
      if (h_counter >= cr_h_srt_sync && h_counter < cr_h_end_sync) begin
        hsync = !cr_hsync_pol;
      end else begin
        hsync = cr_hsync_pol;
      end

      if (v_counter >= cr_v_srt_sync && v_counter < cr_v_end_sync) begin
        vsync = !cr_vsync_pol;
      end else begin
        vsync = cr_vsync_pol;
      end

      video_enable = h_counter < cr_h_end_disp && v_counter < cr_v_end_disp;
      end_of_line_d = h_counter == cr_h_end_disp && v_counter < cr_v_end_disp;
      end_of_frame_d = h_counter == cr_h_end_disp && v_counter == cr_v_end_disp - 1;

      buffer_read_en = h_counter < cr_fb_width_latch && v_counter < cr_fb_height_latch;
      unique case (cr_fb_depth_latch)
        1'b0: buffer_read_addr = {v_counter[0], h_counter[11:0]};
        1'b1: buffer_read_addr = {v_counter[0], 1'b0, h_counter[11:1]};
      endcase
      buffer_read_offset = h_counter[0];
    end
  end

  always_ff @(posedge pxl_clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      cr_enabled <= 1'b0;
      hsync_o <= 1'b0;
      vsync_o <= 1'b0;
      pixel_o <= 1'b0;
      end_of_line <= 1'b0;
      end_of_frame <= 1'b0;
      buffer_read_en_q <= 1'b0;
      buffer_read_offset_q <= 1'b0;
    end else begin
      // These two signals need to be strictly pulse signals for CDC.
      end_of_line <= 1'b0;
      end_of_frame <= 1'b0;

      if (pxl_clk_en_i) begin
        cr_enabled <= cr_enable;
        hsync_o <= hsync;
        vsync_o <= vsync;
        pixel_o <= video_enable;
        end_of_line <= end_of_line_d;
        end_of_frame <= end_of_frame_d;
        buffer_read_en_q <= buffer_read_en;
        buffer_read_offset_q <= buffer_read_offset;
      end
    end
  end

  // Convert r5g6b5 to b8g8r8
  function [23:0] unpack16 (input [15:0] color);
    unpack16 = {
      color[ 4: 0], color[ 4: 2],
      color[10: 5], color[10: 9],
      color[15:11], color[15:13]
    };
  endfunction

  // Compute the correct color output with the data read from line buffer.
  logic [23:0] color;
  always_comb begin
    color = 0;
    if (pixel_o) begin
      color = cr_bg_color_latch;
      if (buffer_read_en_q) begin
        unique case (cr_fb_depth_latch)
          1'b0: color = buffer_read_data;
          1'b1: color = unpack16(buffer_read_offset_q ? buffer_read_data[31:16] : buffer_read_data[15:0]);
        endcase
      end
    end
  end

  assign red_o   = color[ 7: 0];
  assign green_o = color[15: 8];
  assign blue_o  = color[23:16];

  // endregion
  /////////////////////////////////

  ////////////////
  // region DMA //

  `AXI_DECLARE(DataWidth, AddrWidth, IdWidth, buffer);

  // Internal write-only AXI channels that serves as the sink of data mover.
  axi_bram_ctrl # (
    .DataWidth (DataWidth),
    .AddrWidth (AddrWidth),
    .BRAM_ADDR_WIDTH (15 - NonBurstSize)
  ) buffer_ctrl (
    .clk_i,
    .rst_ni,
    `AXI_CONNECT_DEVICE_PORT(host, buffer),
    .bram_en (buffer_write_en),
    .bram_we (buffer_write_strb),
    .bram_addr (buffer_write_addr),
    .bram_wrdata (buffer_write_data),
    .bram_rddata ()
  );

  logic dma_ready;
  logic dma_valid;
  logic [AddrWidth-1:0] dma_src;
  logic [AddrWidth-1:0] dma_dst;
  logic [AddrWidth-1:0] dma_len;

  axi_data_mover # (
    .DataWidth (DataWidth),
    .AddrWidth (AddrWidth),
    .MaxBurstLen (256)
  ) data_mover (
    .clk_i,
    .rst_ni,
    `AXI_FORWARD_HOST_PORT(src, dma),
    `AXI_CONNECT_HOST_PORT(dst, buffer),
    .ready_o (dma_ready),
    .valid_i (dma_valid),
    .src_i (dma_src),
    .dst_i (dma_dst),
    .len_i (dma_len)
  );

  assign dma_len = AddrWidth'(cr_fb_depth_latch ? {cr_fb_width_latch, 1'b0} : {cr_fb_width_latch, 2'b00});

  logic end_of_frame_sync_q;
  logic end_of_line_sync_q;

  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      end_of_frame_sync_q <= 1'b0;
      end_of_line_sync_q <= 1'b0;
      dma_valid <= 1'b0;
      dma_src <= 'x;
      dma_dst <= 'x;
    end else begin
      // Delay the signal by one cycle so that we see the commited values.
      end_of_frame_sync_q <= end_of_frame_sync;
      end_of_line_sync_q <= end_of_line_sync;

      if (dma_valid && dma_ready) begin
        dma_valid <= 1'b0;
        dma_src <= dma_src + cr_fb_bpl_latch;
      end

      // We use BPL = 0 to indicate that framebuffer is disabled.
      if (cr_enable && cr_fb_bpl_latch != 0) begin
        if (end_of_frame_sync_q) begin
          dma_valid <= 1'b1;
          dma_src <= cr_fb_base_latch;
          dma_dst <= 0;
        end else if (end_of_line_sync_q) begin
          dma_valid <= 1'b1;
          // dma_src is incremented already.
          dma_dst <= {!dma_dst[14], 14'd0};
        end
      end
    end
  end

  // endregion
  ////////////////

endmodule
