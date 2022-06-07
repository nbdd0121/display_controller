`include "axi_util.svh"

module axi_data_mover #(
  parameter AddrWidth = 64,
  parameter DataWidth = 64,
  parameter IdWidth = 1,
  parameter MaxBurstLen = 256
) (
  input  logic clk_i,
  input  logic rst_ni,

  `AXI_DECLARE_HOST_PORT(DataWidth, AddrWidth, IdWidth, src),
  `AXI_DECLARE_HOST_PORT(DataWidth, AddrWidth, IdWidth, dst),

  output logic                 ready_o,
  input  logic                 valid_i,
  input  logic [AddrWidth-1:0] src_i,
  input  logic [AddrWidth-1:0] dst_i,
  input  logic [AddrWidth-1:0] len_i
);

  `AXI_DECLARE(DataWidth, AddrWidth, IdWidth, src);
  `AXI_DECLARE(DataWidth, AddrWidth, IdWidth, dst);
  `AXI_BIND_HOST_PORT(src, src);
  `AXI_BIND_HOST_PORT(dst, dst);

  localparam AddrShift = $clog2(DataWidth / 8);

  // Unused channels.
  assign src_aw_valid = 1'b0;
  assign src_aw = 'x;
  assign src_w_valid = 1'b0;
  assign src_w = 'x;
  assign src_b_ready = 1'b0;
  assign dst_ar_valid = 1'b0;
  assign dst_ar = 'x;
  assign dst_r_ready = 1'b0;

  // Connect dst's W channel to src's R channel directly
  // Errors are ignored.
  assign src_r_ready = dst_w_ready;
  assign dst_w_valid = src_r_valid;
  assign dst_w.strb  = '1;
  assign dst_w.data  = src_r.data;
  assign dst_w.last  = src_r.last;

  typedef enum logic [1:0] {
    StateIdle,
    StateAddr,
    StateWait
  } state_e;

  state_e state_q, state_d;
  logic [AddrWidth-1:0] src_addr_q, src_addr_d;
  logic [AddrWidth-1:0] dst_addr_q, dst_addr_d;
  logic [AddrWidth-1:0] len_q, len_d;
  logic src_accepted_q, src_accepted_d;
  logic dst_accepted_q, dst_accepted_d;

  always_comb begin
    state_d = state_q;
    src_addr_d = src_addr_q;
    dst_addr_d = dst_addr_q;
    len_d = len_q;
    src_accepted_d = src_accepted_q;
    dst_accepted_d = dst_accepted_q;

    ready_o = 1'b0;

    src_ar_valid = 1'b0;
    src_ar = 'x;
    src_ar.id = 0;
    src_ar.size = 3'b011;
    src_ar.burst = axi_pkg::BURST_INCR;
    src_ar.cache = 4'b0;
    src_ar.prot = 3'b0;
    src_ar.lock = 1'b0;

    dst_aw_valid = 1'b0;
    dst_aw = 'x;
    dst_aw.id = 0;
    dst_aw.size = 3'b011;
    dst_aw.burst = axi_pkg::BURST_INCR;
    dst_aw.cache = 4'b0;
    dst_aw.prot = 3'b0;
    dst_aw.lock = 1'b0;

    dst_b_ready = 1'b0;

    unique case (state_q)
      StateIdle: begin
        ready_o = 1'b1;

        if (valid_i) begin
          state_d = StateAddr;
          src_addr_d = src_i;
          dst_addr_d = dst_i;
          len_d = len_i;
          src_accepted_d = 1'b0;
          dst_accepted_d = 1'b0;
        end
      end

      StateAddr: begin
        src_ar_valid = !src_accepted_q;
        src_ar.addr = src_addr_q;
        dst_aw_valid = !dst_accepted_q;
        dst_aw.addr = dst_addr_q;

        if ((len_q >> AddrShift) > MaxBurstLen) begin
          src_ar.len = MaxBurstLen - 1;
          dst_aw.len = MaxBurstLen - 1;
        end else begin
          src_ar.len = (len_q >> AddrShift) - 1;
         dst_aw.len = (len_q >> AddrShift) - 1;
        end

        if (src_ar_valid) begin
          src_accepted_d = 1'b1;
        end

        if (dst_aw_valid) begin
          dst_accepted_d = 1'b1;
        end

        if (src_accepted_d && dst_accepted_d) begin
          if ((len_q >> AddrShift) > MaxBurstLen) begin
            src_addr_d = src_addr_q + (MaxBurstLen << AddrShift);
            dst_addr_d = dst_addr_q + (MaxBurstLen << AddrShift);
            len_d = len_q - (MaxBurstLen << AddrShift);
          end else begin
            src_addr_d = 'x;
            dst_addr_d = 'x;
            len_d = 0;
          end

          state_d = StateWait;
        end
      end

      StateWait: begin
        dst_b_ready = 1'b1;

        if (dst_b_valid) begin
          src_accepted_d = 1'b0;
          dst_accepted_d = 1'b0;
          if (len_q == 0) begin
            state_d = StateIdle;
          end else begin
            state_d = StateAddr;
          end
        end
      end
    endcase
  end

  always_ff @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      state_q <= StateIdle;
      src_addr_q <= 'x;
      dst_addr_q <= 'x;
      len_q <= 'x;
      src_accepted_q <= 1'b0;
      dst_accepted_q <= 1'b0;
    end else begin
      state_q <= state_d;
      src_addr_q <= src_addr_d;
      dst_addr_q <= dst_addr_d;
      len_q <= len_d;
      src_accepted_q <= src_accepted_d;
      dst_accepted_q <= dst_accepted_d;
    end
  end

endmodule
