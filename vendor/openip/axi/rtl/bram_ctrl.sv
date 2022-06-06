/*
 * Copyright (c) 2018, Gary Guo
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
//
// HIGH_PERFORMANCE: By default this component runs in high performance mode (which means it can respond to one
//     read/write request every cycle). Set this parameter to 0 will turn off high performance mode, reducing a few
//     register usages.
module axi_bram_ctrl #(
    parameter DataWidth = 64,
    parameter AddrWidth = 64,
    parameter IdWidth = 1,
    parameter BRAM_ADDR_WIDTH,
    parameter HIGH_PERFORMANCE = 1
) (
    input  logic clk_i,
    input  logic rst_ni,

    `AXI_DECLARE_DEVICE_PORT(DataWidth, AddrWidth, IdWidth, host),

    output                       bram_en,
    output [DataWidth/8-1:0]     bram_we,
    output [BRAM_ADDR_WIDTH-1:0] bram_addr,
    output [DataWidth-1:0]       bram_wrdata,
    input  [DataWidth-1:0]       bram_rddata
);

    `AXI_LITE_DECLARE(DataWidth, AddrWidth, ch);

    axi_to_lite #(
        .DataWidth (DataWidth),
        .AddrWidth (AddrWidth),
        .IdWidth (IdWidth),
        .HIGH_PERFORMANCE (HIGH_PERFORMANCE)
    ) bridge (
        .clk_i,
        .rst_ni,
        `AXI_FORWARD_DEVICE_PORT(host, host),
        `AXI_CONNECT_HOST_PORT(device, ch)
    );

    axi_lite_bram_ctrl #(
        .DataWidth (DataWidth),
        .AddrWidth (AddrWidth),
        .BRAM_ADDR_WIDTH (BRAM_ADDR_WIDTH)
    ) controller (
        .clk_i,
        .rst_ni,
        `AXI_CONNECT_DEVICE_PORT(host, ch),
        .bram_en,
        .bram_we,
        .bram_addr,
        .bram_wrdata,
        .bram_rddata
    );

endmodule