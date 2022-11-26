`include "tl_util.svh"

module display_controller_data_mover import tl_pkg::*; #(
  parameter AddrWidth = 64,
  parameter DataWidth = 64,
  parameter SourceWidth = 1,
  parameter BlockSize = 6,
  parameter DstFifo = 1'b1
) (
  input  logic clk_i,
  input  logic rst_ni,

  `TL_DECLARE_HOST_PORT(DataWidth, AddrWidth, SourceWidth, 1, src),
  `TL_DECLARE_HOST_PORT(DataWidth, AddrWidth, SourceWidth, 1, dst),

  output logic                 ready_o,
  input  logic                 valid_i,
  input  logic [AddrWidth-1:0] src_i,
  input  logic [AddrWidth-1:0] dst_i,
  input  logic [AddrWidth-1:0] len_i
);

  `TL_DECLARE(DataWidth, AddrWidth, AddrWidth - BlockSize, 1, src);
  `TL_DECLARE(DataWidth, AddrWidth, SourceWidth, 1, dst);

  tl_adapter #(
    .DataWidth   (DataWidth),
    .AddrWidth   (AddrWidth),
    .HostSourceWidth (AddrWidth - BlockSize),
    .DeviceSourceWidth (SourceWidth),
    .SinkWidth   (1),
    .MaxSize     (BlockSize),
    .HostFifo    (1'b1),
    .DeviceFifo  (1'b0)
  ) src_adapter (
    .clk_i,
    .rst_ni,
    `TL_CONNECT_DEVICE_PORT(host, src),
    `TL_FORWARD_HOST_PORT(device, src)
  );

  tl_adapter #(
    .DataWidth   (DataWidth),
    .AddrWidth   (AddrWidth),
    .SourceWidth (SourceWidth),
    .SinkWidth   (1),
    .MaxSize     (BlockSize),
    .HostFifo    (1'b1),
    .DeviceFifo  (DstFifo)
  ) dst_adapter (
    .clk_i,
    .rst_ni,
    `TL_CONNECT_DEVICE_PORT(host, dst),
    `TL_FORWARD_HOST_PORT(device, dst)
  );

  localparam DataWidthInBytes = DataWidth / 8;
  localparam NonBurstSize = $clog2(DataWidthInBytes);

  // Unused channels.
  assign src_b_ready = 1'b1;
  assign src_c_valid = 1'b0;
  assign src_c       = 'x;
  assign src_e_valid = 1'b0;
  assign src_e       = 'x;
  assign dst_b_ready = 1'b1;
  assign dst_c_valid = 1'b0;
  assign dst_c       = 'x;
  assign dst_e_valid = 1'b0;
  assign dst_e       = 'x;

  typedef enum logic [1:0] {
    StateIdle,
    StateAddr,
    StateWait
  } state_e;

  state_e state_q, state_d;
  logic [AddrWidth-BlockSize-1:0] src_addr_q, src_addr_d;
  logic [AddrWidth-BlockSize-1:0] dst_addr_q, dst_addr_d;
  logic [AddrWidth-BlockSize-1:0] len_q, len_d;

  always_comb begin
    state_d = state_q;
    src_addr_d = src_addr_q;
    dst_addr_d = dst_addr_q;
    len_d = len_q;

    ready_o = 1'b0;

    src_a_valid = 1'b0;
    src_a = 'x;

    // Pipe data read to data write.
    src_d_ready   = dst_a_ready;
    dst_a_valid   = src_d_valid;
    dst_a.opcode  = PutFullData;
    dst_a.param   = 0;
    dst_a.size    = BlockSize;
    dst_a.source  = 0;
    dst_a.address = {src_d.source, {BlockSize{1'b0}}};
    dst_a.mask    = '1;
    dst_a.corrupt = src_d.corrupt;
    dst_a.data    = src_d.data;

    // We don't care about write responses, okay as dst responds in FIFO order
    // (otherwise we would have a FIFO converter in between).
    dst_d_ready = 1'b1;

    unique case (state_q)
      StateIdle: begin
        ready_o = 1'b1;

        if (valid_i) begin
          state_d = StateAddr;
          src_addr_d = src_i[AddrWidth-1:BlockSize];
          dst_addr_d = dst_i[AddrWidth-1:BlockSize];
          len_d = len_i[AddrWidth-1:BlockSize];
        end
      end

      StateAddr: begin
        src_a_valid   = 1'b1;
        src_a.opcode  = Get;
        src_a.param   = 0;
        // This data mover only supports block transfers.
        src_a.size    = BlockSize;
        // The source here don't have to be unique, because we have a FIFO converter.
        src_a.source  = dst_addr_q;
        src_a.address = {src_addr_q, {BlockSize{1'b0}}};
        // Assume block size is larger than non burst size.
        src_a.mask    = '1;
        src_a.corrupt = 1'b0;
        src_a.data    = 'x;

        if (src_a_ready) begin
          src_addr_d = src_addr_q + 1;
          dst_addr_d = dst_addr_q + 1;
          len_d = len_q - 1;
          if (len_d == 0) begin
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
    end else begin
      state_q <= state_d;
      src_addr_q <= src_addr_d;
      dst_addr_q <= dst_addr_d;
      len_q <= len_d;
    end
  end

endmodule
