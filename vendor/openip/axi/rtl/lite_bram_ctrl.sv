/*
 * Copyright (c) 2018-2022, Gary Guo
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *  * Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

`include "axi_util.svh"
`include "axi_lite_util.svh"

// A component that converts an AXI-lite interface to a BRAM interface.
module axi_lite_bram_ctrl #(
    parameter DataWidth = 64,
    parameter AddrWidth = 64,
    parameter BRAM_ADDR_WIDTH
) (
    input  logic clk_i,
    input  logic rst_ni,

    `AXI_LITE_DECLARE_DEVICE_PORT(DataWidth, AddrWidth, host),

    output                       bram_en,
    output [DataWidth/8-1:0]     bram_we,
    output [BRAM_ADDR_WIDTH-1:0] bram_addr,
    output [DataWidth-1:0]       bram_wrdata,
    input  [DataWidth-1:0]       bram_rddata
);

    `AXI_LITE_DECLARE(DataWidth, AddrWidth, host);
    `AXI_BIND_DEVICE_PORT(host, host);

    localparam STRB_WIDTH        = DataWidth / 8;
    localparam UNUSED_ADDR_WIDTH = $clog2(STRB_WIDTH);

    // Static checks of interface matching
    // We currently don't strictly enforce UNUSED_ADDR_WIDTH + BRAM_ADDR_WIDTH == AddrWidth and use truncation
    // behaviour instead.
    if (UNUSED_ADDR_WIDTH + BRAM_ADDR_WIDTH > AddrWidth)
        $fatal(1, "ADDR_WIDTH DataWidth mismatch");

    // High-level description of how this module works:
    // This BRAM controller only has a single port for both read/write, which means that it needs to serialise
    // concurrent read/write request. Further more, it needs to deliver both address and write data to the BRAM port in
    // the same cycle.
    // What we do here is to use combinatorial paths to make sure that both write address and write data channel will
    // fire in the same cycle. We also use combinatorial path to stop read address channel from firing when we will be
    // dealing with a write request.
    // The design choice we made makes the module really easy, but it introduces a lot combinatorial paths between
    // AR, AW and W channels. This is forbidden by AXI as it may potentially cause wire loops. Therefore we use
    // register slices to break the dependencies.

    //
    // Break combinatorial path between ready signals.
    //

    logic                       aw_valid;
    logic                       aw_ready;
    logic [BRAM_ADDR_WIDTH-1:0] aw_addr;
    openip_regslice #(
        .DATA_WIDTH   (BRAM_ADDR_WIDTH),
        .FORWARD      (1'b0)
    ) awfifo (
        .clk     (clk_i),
        .rstn    (rst_ni),
        .w_valid (host_aw_valid),
        .w_ready (host_aw_ready),
        .w_data  (host_aw.addr[UNUSED_ADDR_WIDTH +: BRAM_ADDR_WIDTH]),
        .r_valid (aw_valid),
        .r_ready (aw_ready),
        .r_data  (aw_addr)
    );

    logic                  w_valid;
    logic                  w_ready;
    logic [DataWidth-1:0]  w_data;
    logic [STRB_WIDTH-1:0] w_strb;

    typedef struct packed {
        logic [DataWidth-1:0] data;
        logic [STRB_WIDTH-1:0] strb;
    } w_pack_t;

    openip_regslice #(
        .TYPE         (w_pack_t),
        .FORWARD      (1'b0)
    ) wfifo (
        .clk     (clk_i),
        .rstn    (rst_ni),
        .w_valid (host_w_valid),
        .w_ready (host_w_ready),
        .w_data  (w_pack_t'{host_w.data, host_w.strb}),
        .r_valid (w_valid),
        .r_ready (w_ready),
        .r_data  ({w_data, w_strb})
    );

    logic                       ar_valid;
    logic                       ar_ready;
    logic [BRAM_ADDR_WIDTH-1:0] ar_addr;
    openip_regslice #(
        .DATA_WIDTH   (BRAM_ADDR_WIDTH),
        .FORWARD      (1'b0)
    ) arfifo (
        .clk     (clk_i),
        .rstn    (rst_ni),
        .w_valid (host_ar_valid),
        .w_ready (host_ar_ready),
        .w_data  (host_ar.addr[UNUSED_ADDR_WIDTH +: BRAM_ADDR_WIDTH]),
        .r_valid (ar_valid),
        .r_ready (ar_ready),
        .r_data  (ar_addr)
    );

    //
    // Logic about whether a transaction happens.
    //

    logic can_write;
    logic do_write;
    logic can_read;

    // We can accept write transaction provided that we can place something into B channel next clock cycle.
    // i.e. b_valid is deasserted (i.e. no data in channel) or b_ready is asserted (i.e. data is going to be consumed).
    assign can_write = !host_b_valid || host_b_ready;
    // A transaction can only take place if both AW and W are ready. This looks scary but is actually allowed since
    // we have placed register slices for both AW and W channels.
    assign aw_ready  = w_valid && can_write;
    assign w_ready   = aw_valid && can_write;
    // Write transaction happens if all three conditions are met.
    assign do_write  = w_valid && aw_valid && can_write;
    // We prioritise write to read, so read cannot happen if we are going to do a write. Otherwise we can do a read
    // if R channel is available.
    assign can_read  = !host_r_valid || host_r_ready;
    assign ar_ready  = !do_write && can_read;

    //
    // Connection to BRAM
    //

    assign bram_en = do_write || (can_read && ar_valid);
    assign bram_addr = do_write ? aw_addr : ar_addr;
    assign bram_we = do_write ? w_strb : '0;
    assign bram_wrdata = w_data;

    //
    // Write handling logic
    //

    assign host_b.resp = axi_pkg::RESP_OKAY;

    always_ff @(posedge clk_i or negedge rst_ni)
        if (!rst_ni) begin
            host_b_valid <= 1'b0;
        end
        else begin
            if (host_b_valid && host_b_ready) begin
                host_b_valid <= 1'b0;
            end
            if (do_write) begin
                host_b_valid <= 1'b1;
            end
        end

    //
    // Read handling logic
    //

    // As bram_rddata is only guaranteed to be available for 1 cycle, we need additional buffer.
    logic                 r_latched;
    logic [DataWidth-1:0] r_latched_data;
    assign host_r.data = r_latched ? r_latched_data : bram_rddata;
    assign host_r.resp = axi_pkg::RESP_OKAY;

    always_ff @(posedge clk_i or negedge rst_ni)
        if (!rst_ni) begin
            r_latched <= 1'b0;
            r_latched_data <= 'x;
            host_r_valid <= 1'b0;
        end
        else begin
            if (host_r_valid) begin
                if (host_r_ready) begin
                    // Buffered data is consumed.
                    r_latched <= 1'b0;
                    r_latched_data <= 'x;
                    host_r_valid <= 1'b0;
                end
                else if (!r_latched) begin
                    // Buffer bram_rddata
                    r_latched <= 1'b1;
                    r_latched_data <= bram_rddata;
                end
            end
            if (ar_valid && ar_ready) begin
                // This will potentially overwrite the `host_r_valid <= 1'b0` above, and is intended.
                host_r_valid <= 1'b1;
            end
        end

endmodule
