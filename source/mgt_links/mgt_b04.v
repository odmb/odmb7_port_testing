//------------------------------------------------------------------------------
//  (c) Copyright 2013-2018 Xilinx, Inc. All rights reserved.
//
//  This file contains confidential and proprietary information
//  of Xilinx, Inc. and is protected under U.S. and
//  international copyright and other intellectual property
//  laws.
//
//  DISCLAIMER
//  This disclaimer is not a license and does not grant any
//  rights to the materials distributed herewith. Except as
//  otherwise provided in a valid license issued to you by
//  Xilinx, and to the maximum extent permitted by applicable
//  law: (1) THESE MATERIALS ARE MADE AVAILABLE "AS IS" AND
//  WITH ALL FAULTS, AND XILINX HEREBY DISCLAIMS ALL WARRANTIES
//  AND CONDITIONS, EXPRESS, IMPLIED, OR STATUTORY, INCLUDING
//  BUT NOT LIMITED TO WARRANTIES OF MERCHANTABILITY, NON-
//  INFRINGEMENT, OR FITNESS FOR ANY PARTICULAR PURPOSE; and
//  (2) Xilinx shall not be liable (whether in contract or tort,
//  including negligence, or under any other theory of
//  liability) for any loss or damage of any kind or nature
//  related to, arising under or in connection with these
//  materials, including for any direct, or any indirect,
//  special, incidental, or consequential loss or damage
//  (including loss of data, profits, goodwill, or any type of
//  loss or damage suffered as a result of any action brought
//  by a third party) even if such damage or loss was
//  reasonably foreseeable or Xilinx had been advised of the
//  possibility of the same.
//
//  CRITICAL APPLICATIONS
//  Xilinx products are not designed or intended to be fail-
//  safe, or for use in any application requiring fail-safe
//  performance, such as life-support or safety devices or
//  systems, Class III medical devices, nuclear facilities,
//  applications related to the deployment of airbags, or any
//  other applications that could lead to death, personal
//  injury, or severe property or environmental damage
//  (individually and collectively, "Critical
//  Applications"). Customer assumes the sole risk and
//  liability of any use of Xilinx products in Critical
//  Applications, subject only to applicable laws and
//  regulations governing limitations on product liability.
//
//  THIS COPYRIGHT NOTICE AND DISCLAIMER MUST BE RETAINED AS
//  PART OF THIS FILE AT ALL TIMES.
//------------------------------------------------------------------------------


`timescale 1ps/1ps

// =====================================================================================================================
// This example design top module instantiates the example design wrapper; slices vectored ports for per-channel
// assignment; and instantiates example resources such as buffers, pattern generators, and pattern checkers for core
// demonstration purposes
// =====================================================================================================================

module mgt_b04 (

  // Serial data ports for transceiver channel 0
  input  wire ch0_gthrxn_in,
  input  wire ch0_gthrxp_in,
  output wire ch0_gthtxn_out,
  output wire ch0_gthtxp_out,

  // Serial data ports for transceiver channel 1
  input  wire ch1_gthrxn_in,
  input  wire ch1_gthrxp_in,
  output wire ch1_gthtxn_out,
  output wire ch1_gthtxp_out,

  // Serial data ports for transceiver channel 2
  input  wire ch2_gthrxn_in,
  input  wire ch2_gthrxp_in,
  output wire ch2_gthtxn_out,
  output wire ch2_gthtxp_out,

  // Serial data ports for transceiver channel 3
  input  wire ch3_gthrxn_in,
  input  wire ch3_gthrxp_in,
  output wire ch3_gthtxn_out,
  output wire ch3_gthtxp_out,

  // TX Data 
  input  wire [15:0] txdata,  // Data to be transmitted
  input  wire txd_valid,

  // Clocks
  input  wire mgtrefclk,
  input  wire sysclk,
  input  wire clk80,
  output wire dduclk,
  output wire txusrclk,
  
  // Reset
  input wire reset

);

  // ===================================================================================================================
  // PER-CHANNEL SIGNAL ASSIGNMENTS
  // ===================================================================================================================

  // The core and example design wrapper vectorize ports across all enabled transceiver channel and common instances for
  // simplicity and compactness. This example design top module assigns slices of each vector to individual, per-channel
  // signal vectors for use if desired. Signals which connect to helper blocks are prefixed "hb#", signals which connect
  // to transceiver common primitives are prefixed "cm#", and signals which connect to transceiver channel primitives
  // are prefixed "ch#", where "#" is the sequential resource number.

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] gthrxn_int;
  assign gthrxn_int[0:0] = ch0_gthrxn_in;
  assign gthrxn_int[1:1] = ch1_gthrxn_in;
  assign gthrxn_int[2:2] = ch2_gthrxn_in;
  assign gthrxn_int[3:3] = ch3_gthrxn_in;

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] gthrxp_int;
  assign gthrxp_int[0:0] = ch0_gthrxp_in;
  assign gthrxp_int[1:1] = ch1_gthrxp_in;
  assign gthrxp_int[2:2] = ch2_gthrxp_in;
  assign gthrxp_int[3:3] = ch3_gthrxp_in;

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] gthtxn_int;
  assign ch0_gthtxn_out = gthtxn_int[0:0];
  assign ch1_gthtxn_out = gthtxn_int[1:1];
  assign ch2_gthtxn_out = gthtxn_int[2:2];
  assign ch3_gthtxn_out = gthtxn_int[3:3];

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] gthtxp_int;
  assign ch0_gthtxp_out = gthtxp_int[0:0];
  assign ch1_gthtxp_out = gthtxp_int[1:1];
  assign ch2_gthtxp_out = gthtxp_int[2:2];
  assign ch3_gthtxp_out = gthtxp_int[3:3];

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_userclk_tx_reset_int;
  wire [0:0] hb0_gtwiz_userclk_tx_reset_int;
  assign gtwiz_userclk_tx_reset_int[0:0] = hb0_gtwiz_userclk_tx_reset_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_userclk_tx_srcclk_int;
  wire [0:0] hb0_gtwiz_userclk_tx_srcclk_int;
  assign hb0_gtwiz_userclk_tx_srcclk_int = gtwiz_userclk_tx_srcclk_int[0:0];

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_userclk_tx_usrclk_int;
  wire [0:0] hb0_gtwiz_userclk_tx_usrclk_int;
  assign hb0_gtwiz_userclk_tx_usrclk_int = gtwiz_userclk_tx_usrclk_int[0:0];

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_userclk_tx_usrclk2_int;
  wire [0:0] hb0_gtwiz_userclk_tx_usrclk2_int;
  assign hb0_gtwiz_userclk_tx_usrclk2_int = gtwiz_userclk_tx_usrclk2_int[0:0];
  assign txusrclk = gtwiz_userclk_tx_usrclk2_int[0:0];
  assign dduclk = clk80;

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_userclk_tx_active_int;
  wire [0:0] hb0_gtwiz_userclk_tx_active_int;
  assign hb0_gtwiz_userclk_tx_active_int = gtwiz_userclk_tx_active_int[0:0];

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_userclk_rx_reset_int;
  wire [0:0] hb0_gtwiz_userclk_rx_reset_int;
  assign gtwiz_userclk_rx_reset_int[0:0] = hb0_gtwiz_userclk_rx_reset_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_userclk_rx_srcclk_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_userclk_rx_usrclk_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_userclk_rx_usrclk2_int;
  wire [0:0] hb0_gtwiz_userclk_rx_usrclk2_int;
  assign hb0_gtwiz_userclk_rx_usrclk2_int = gtwiz_userclk_rx_usrclk2_int[0:0];

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_userclk_rx_active_int;
  wire [0:0] hb0_gtwiz_userclk_rx_active_int;
  assign hb0_gtwiz_userclk_rx_active_int = gtwiz_userclk_rx_active_int[0:0];

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_reset_tx_pll_and_datapath_int;
  wire [0:0] gtwiz_reset_tx_datapath_int;
  wire [0:0] gtwiz_reset_rx_cdr_stable_int;
  wire [0:0] gtwiz_reset_tx_done_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtwiz_reset_rx_done_int;
  wire [0:0] hb0_gtwiz_reset_rx_done_int;
  assign hb0_gtwiz_reset_rx_done_int = gtwiz_reset_rx_done_int[0:0];

  //--------------------------------------------------------------------------------------------------------------------
  wire [127:0] gtwiz_userdata_tx_int;
  wire [31:0] hb0_gtwiz_userdata_tx_int;
  wire [31:0] hb1_gtwiz_userdata_tx_int;
  wire [31:0] hb2_gtwiz_userdata_tx_int;
  wire [31:0] hb3_gtwiz_userdata_tx_int;
  assign gtwiz_userdata_tx_int[31:0] = hb0_gtwiz_userdata_tx_int;
  assign gtwiz_userdata_tx_int[63:32] = hb1_gtwiz_userdata_tx_int;
  assign gtwiz_userdata_tx_int[95:64] = hb2_gtwiz_userdata_tx_int;
  assign gtwiz_userdata_tx_int[127:96] = hb3_gtwiz_userdata_tx_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [127:0] gtwiz_userdata_rx_int;
  wire [31:0] hb0_gtwiz_userdata_rx_int;
  wire [31:0] hb1_gtwiz_userdata_rx_int;
  wire [31:0] hb2_gtwiz_userdata_rx_int;
  wire [31:0] hb3_gtwiz_userdata_rx_int;
  assign hb0_gtwiz_userdata_rx_int = gtwiz_userdata_rx_int[31:0];
  assign hb1_gtwiz_userdata_rx_int = gtwiz_userdata_rx_int[63:32];
  assign hb2_gtwiz_userdata_rx_int = gtwiz_userdata_rx_int[95:64];
  assign hb3_gtwiz_userdata_rx_int = gtwiz_userdata_rx_int[127:96];

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] gtrefclk00_int;
  wire [0:0] cm0_gtrefclk00_int;
  assign gtrefclk00_int[0:0] = cm0_gtrefclk00_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] qpll0outclk_int;
  wire [0:0] cm0_qpll0outclk_int;
  assign cm0_qpll0outclk_int = qpll0outclk_int[0:0];

  //--------------------------------------------------------------------------------------------------------------------
  wire [0:0] qpll0outrefclk_int;
  wire [0:0] cm0_qpll0outrefclk_int;
  assign cm0_qpll0outrefclk_int = qpll0outrefclk_int[0:0];

  //--------------------------------------------------------------------------------------------------------------------
  wire [11:0] loopback_int;
  // This vector is not sliced because it is directly assigned in a debug core instance below

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] rxbufreset_int = 4'b0000;
  wire [3:0] rxcommadeten_int = 4'b1111;
  wire [3:0] rxlpmen_int = 4'b1111;

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] rxmcommaalignen_int = 4'b1111;
  wire [3:0] rxpcommaalignen_int = 4'b1111;

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] rx8b10ben_int = 4'b1111;
  wire [3:0] tx8b10ben_int = 4'b1111;

  //--------------------------------------------------------------------------------------------------------------------
  wire [63:0] txctrl0_int;
  wire [63:0] txctrl1_int;
  wire [31:0] txctrl2_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] rxpolarity_int = 4'b0000;
  wire [3:0] txpolarity_int = 4'b0000;

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] gtpowergood_int;
  wire [11:0] rxbufstatus_int;
  wire [7:0] rxclkcorcnt_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] rxcommadet_int;
  wire [3:0] rxbyteisaligned_int;
  wire [3:0] rxbyterealign_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [63:0] rxctrl0_int;
  wire [63:0] rxctrl1_int;
  wire [31:0] rxctrl2_int;
  wire [31:0] rxctrl3_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [3:0] rxpmaresetdone_int;
  wire [3:0] txpmaresetdone_int;

  // ===================================================================================================================
  // Channel Bonding logic
  // ===================================================================================================================
  //Connect the daisy chaining as appropriate for the protocol, below
  //connections are indicative and connect master lane to all the other lanes

  wire [3:0] rxchanbondseq_int;
  wire [3:0] rxchanisaligned_int;
  wire [3:0] rxchanrealign_int;

  wire [3:0] rxchbondslave_int = 4'b1101;
  wire [3:0] rxchbondmaster_int = 4'b0010;
  wire [3:0] rxchbonden_int = 4'b1111;

  //--------------------------------------------------------------------------------------------------------------------
  wire [19:0] rxchbondi_int;
  wire [4:0] ch0_rxchbondi_int;
  wire [4:0] ch1_rxchbondi_int;
  wire [4:0] ch2_rxchbondi_int;
  wire [4:0] ch3_rxchbondi_int;
  assign rxchbondi_int[4:0] = ch0_rxchbondi_int;
  assign rxchbondi_int[9:5] = ch1_rxchbondi_int;
  assign rxchbondi_int[14:10] = ch2_rxchbondi_int;
  assign rxchbondi_int[19:15] = ch3_rxchbondi_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [11:0] rxchbondlevel_int;
  wire [2:0] ch0_rxchbondlevel_int;
  wire [2:0] ch1_rxchbondlevel_int;
  wire [2:0] ch2_rxchbondlevel_int;
  wire [2:0] ch3_rxchbondlevel_int;
  assign rxchbondlevel_int[2:0] = ch0_rxchbondlevel_int;
  assign rxchbondlevel_int[5:3] = ch1_rxchbondlevel_int;
  assign rxchbondlevel_int[8:6] = ch2_rxchbondlevel_int;
  assign rxchbondlevel_int[11:9] = ch3_rxchbondlevel_int;

  //--------------------------------------------------------------------------------------------------------------------
  wire [19:0] rxchbondo_int;
  wire [4:0] ch0_rxchbondo_int;
  wire [4:0] ch1_rxchbondo_int;
  wire [4:0] ch2_rxchbondo_int;
  wire [4:0] ch3_rxchbondo_int;
  assign ch0_rxchbondo_int = rxchbondo_int[4:0];
  assign ch1_rxchbondo_int = rxchbondo_int[9:5];
  assign ch2_rxchbondo_int = rxchbondo_int[14:10];
  assign ch3_rxchbondo_int = rxchbondo_int[19:15];

  assign ch0_rxchbondi_int = ch1_rxchbondo_int;
  assign ch0_rxchbondlevel_int = 3'd1;
  assign ch1_rxchbondi_int = 5'd0;
  assign ch1_rxchbondlevel_int = 3'd2;
  assign ch2_rxchbondi_int = ch1_rxchbondo_int;
  assign ch2_rxchbondlevel_int = 3'd1;
  assign ch3_rxchbondi_int = ch2_rxchbondo_int;
  assign ch3_rxchbondlevel_int = 3'd0;
 
  // ===================================================================================================================
  // BUFFERS
  // ===================================================================================================================

  // Reset
  wire hb_gtwiz_reset_all_int;
  assign hb_gtwiz_reset_all_int = reset;

  // Globally buffer the free-running input clock
  wire hb_gtwiz_reset_clk_freerun_buf_int;
  assign hb_gtwiz_reset_clk_freerun_in = sysclk;

  BUFG bufg_clk_freerun_inst (
    .I (hb_gtwiz_reset_clk_freerun_in),
    .O (hb_gtwiz_reset_clk_freerun_buf_int)
  );

  // MGT RefClk
  assign cm0_gtrefclk00_int = mgtrefclk;


  // ===================================================================================================================
  // USER CLOCKING RESETS
  // ===================================================================================================================

  // The TX user clocking helper block should be held in reset until the clock source of that block is known to be
  // stable. The following assignment is an example of how that stability can be determined, based on the selected TX
  // user clock source. Replace the assignment with the appropriate signal or logic to achieve that behavior as needed.
  assign hb0_gtwiz_userclk_tx_reset_int = ~(&txpmaresetdone_int);

  // The RX user clocking helper block should be held in reset until the clock source of that block is known to be
  // stable. The following assignment is an example of how that stability can be determined, based on the selected RX
  // user clock source. Replace the assignment with the appropriate signal or logic to achieve that behavior as needed.
  assign hb0_gtwiz_userclk_rx_reset_int = ~(&rxpmaresetdone_int);


  // ===================================================================================================================
  // PRBS STIMULUS, CHECKING, AND LINK MANAGEMENT
  // ===================================================================================================================

  // PRBS stimulus
  // -------------------------------------------------------------------------------------------------------------------

  // For 8B/10B mode data transmission, the PRBS generator is always enabled
  reg [31:0] idle_cntdown  = 32'd0;

  // -------------------------------------------------------------------------------------------------------------------
  // Reset synchronizer
  // -------------------------------------------------------------------------------------------------------------------

  // Synchronize the example stimulus reset condition into the txusrclk2 domain
  wire example_stimulus_reset_int = hb_gtwiz_reset_all_int || ~hb0_gtwiz_reset_rx_done_int || ~hb0_gtwiz_userclk_tx_active_int;
  wire example_stimulus_reset_sync;

  (* DONT_TOUCH = "TRUE" *)
  gtwizard_ultrascale_0_example_reset_synchronizer example_stimulus_reset_synchronizer_inst (
    .clk_in  (gtwiz_userclk_tx_usrclk2_in),
    .rst_in  (example_stimulus_reset_int),
    .rst_out (example_stimulus_reset_sync)
  );

  // Declare a txdata vector to be driven by the PRBS generator, a txdata vector to be driven by a comma alignment
  // character, and a txdata register that is used to synchronously capture and drive one of the two. Perform other
  // continuous assignments required for this use mode.
  wire   [31:0] idle_word = {32'h505050BC};
  wire   [31:0] chan_bond_word = {32'h606060BC};
  reg    [31:0] txdata_reg  = 32'b0;
  reg    [31:0] txdata_reg2 = 32'b0;
  reg    [7:0]  txctrl2_reg = 8'b0;
  assign        txctrl0_int = 64'b0;
  assign        txctrl1_int = 64'b0;

  // Simply drive the comma alignment character for a short period of time upon bring-up to allow the receiver to align,
  // and then continually select the PRBS data afterwards. This provides a continuous stream of PRBS data to the aligned
  // receiver. Note that this simple mechanism assumes the receiver is operating by the time the prbs_slt_ctr counter
  // has saturated. This is sufficient for loopback demonstration of this example design, as this example stimulus
  // module is also held in reset until the receiver completes its reset sequence; but chip-to-chip or more complex
  // adaptations of this example design may require modifications for robustness.
  reg [0:0]  tx_idle_char = 1'b0;     
  always @(posedge hb0_gtwiz_userclk_tx_usrclk2_int) begin
    if (example_stimulus_reset_sync) begin
      if (tx_idle_char == 1'b0)
        txdata_reg <= chan_bond_word;
      else
        txdata_reg <= idle_word;
      txctrl2_reg  <= 8'b1;
      tx_idle_char <= ~tx_idle_char;
    end
    else begin
      if (txd_valid) begin
        txdata_reg  <= {txdata, 16'b0};
        txdata_reg2  <= 32'b0;
        txctrl2_reg <= 8'b0;
      end
      else begin
        if (tx_idle_char == 1'b0) begin
          txdata_reg  <= chan_bond_word;
          txdata_reg2 <= chan_bond_word;
        end else begin
          txdata_reg  <= idle_word;
          txdata_reg2 <= idle_word;
        end
        txctrl2_reg  <= 8'b1;
        tx_idle_char <= ~tx_idle_char;
      end
    end
  end

  assign txctrl2_int = {4{txctrl2_reg}};

  assign hb0_gtwiz_userdata_tx_int = txdata_reg;
  assign hb1_gtwiz_userdata_tx_int = txdata_reg2;
  assign hb2_gtwiz_userdata_tx_int = txdata_reg2;
  assign hb3_gtwiz_userdata_tx_int = txdata_reg2;

  // ===================================================================================================================
  // INITIALIZATION
  // ===================================================================================================================

  // Declare the receiver reset signals that interface to the reset controller helper block. For this configuration,
  // which uses the same PLL type for transmitter and receiver, the "reset RX PLL and datapath" feature is not used.
  wire hb_gtwiz_reset_rx_pll_and_datapath_int = 1'b0;
  wire hb_gtwiz_reset_rx_datapath_int;

  // ===================================================================================================================
  // ILA FOR HARDWARE BRING-UP AND DEBUG
  // ===================================================================================================================

  wire [841:0] ila_data; 
  
  assign ila_data[127:0] = gtwiz_userdata_rx_int;
  assign ila_data[255:128] = gtwiz_userdata_tx_int;
  assign ila_data[260] = hb0_gtwiz_userclk_rx_usrclk2_int;
  assign ila_data[261] = hb0_gtwiz_userclk_rx_active_int;
  assign ila_data[262] = hb_gtwiz_reset_all_int;
  assign ila_data[263] = hb0_gtwiz_reset_rx_done_int;
  assign ila_data[264] = hb0_gtwiz_userclk_tx_usrclk2_int;
  assign ila_data[265] = hb0_gtwiz_userclk_tx_active_int;
  assign ila_data[269:266] = rx8b10ben_int;
  assign ila_data[273:270] = rxcommadeten_int;
  assign ila_data[277:274] = rxlpmen_int;
  assign ila_data[281:278] = rxmcommaalignen_int;
  assign ila_data[285:282] = rxpcommaalignen_int;
  assign ila_data[289:286] = tx8b10ben_int;
  assign ila_data[293:290] = gtpowergood_int;
  assign ila_data[297:294] = rxbyteisaligned_int;
  assign ila_data[301:298] = rxbyterealign_int;
  assign ila_data[305:302] = rxcommadet_int;
  assign ila_data[309:306] = rxpmaresetdone_int;
  assign ila_data[313:310] = txpmaresetdone_int;  
  assign ila_data[377:314] = rxctrl0_int;
  assign ila_data[441:378] = rxctrl1_int;
  assign ila_data[473:442] = rxctrl2_int;
  assign ila_data[505:474] = rxctrl3_int;
  assign ila_data[569:506] = txctrl0_int;
  assign ila_data[633:570] = txctrl1_int;
  assign ila_data[665:634] = txctrl2_int;
  assign ila_data[773:770] = rxchanisaligned_int;
  assign ila_data[777:774] = rxchanrealign_int;
  assign ila_data[781:778] = rxchanbondseq_int;
  assign ila_data[801:782] = rxchbondi_int;
  assign ila_data[821:802] = rxchbondo_int;
  assign ila_data[833:822] = rxbufstatus_int;
  assign ila_data[841:834] = rxclkcorcnt_int;

  ila_0 ila_i (
    .clk(hb0_gtwiz_userclk_rx_usrclk2_int), // input wire clk
    //.probe0(ila_trigger), // input wire [7:0]  probe0
    .probe0(ila_data) // input wire [255:0]  probe1
  );

  // ===================================================================================================================
  // EXAMPLE WRAPPER INSTANCE
  // ===================================================================================================================

  // Instantiate the example design wrapper, mapping its enabled ports to per-channel internal signals and example
  // resources as appropriate
  gtwizard_ultrascale_0_example_wrapper example_wrapper_inst (
    .gthrxn_in                               (gthrxn_int)
   ,.gthrxp_in                               (gthrxp_int)
   ,.gthtxn_out                              (gthtxn_int)
   ,.gthtxp_out                              (gthtxp_int)
   ,.gtwiz_userclk_tx_reset_in               (gtwiz_userclk_tx_reset_int)
   ,.gtwiz_userclk_tx_srcclk_out             (gtwiz_userclk_tx_srcclk_int)
   ,.gtwiz_userclk_tx_usrclk_out             (gtwiz_userclk_tx_usrclk_int)
   ,.gtwiz_userclk_tx_usrclk2_out            (gtwiz_userclk_tx_usrclk2_int)
   ,.gtwiz_userclk_tx_active_out             (gtwiz_userclk_tx_active_int)
   ,.gtwiz_userclk_rx_reset_in               (gtwiz_userclk_rx_reset_int)
   ,.gtwiz_userclk_rx_srcclk_out             (gtwiz_userclk_rx_srcclk_int)
   ,.gtwiz_userclk_rx_usrclk_out             (gtwiz_userclk_rx_usrclk_int)
   ,.gtwiz_userclk_rx_usrclk2_out            (gtwiz_userclk_rx_usrclk2_int)
   ,.gtwiz_userclk_rx_active_out             (gtwiz_userclk_rx_active_int)
   ,.gtwiz_reset_clk_freerun_in              ({1{hb_gtwiz_reset_clk_freerun_buf_int}})
   ,.gtwiz_reset_all_in                      ({1{hb_gtwiz_reset_all_int}})
   ,.gtwiz_reset_tx_pll_and_datapath_in      (gtwiz_reset_tx_pll_and_datapath_int)
   ,.gtwiz_reset_tx_datapath_in              (gtwiz_reset_tx_datapath_int)
   ,.gtwiz_reset_rx_pll_and_datapath_in      ({1{hb_gtwiz_reset_rx_pll_and_datapath_int}})
   ,.gtwiz_reset_rx_datapath_in              ({1{hb_gtwiz_reset_rx_datapath_int}})
   ,.gtwiz_reset_rx_cdr_stable_out           (gtwiz_reset_rx_cdr_stable_int)
   ,.gtwiz_reset_tx_done_out                 (gtwiz_reset_tx_done_int)
   ,.gtwiz_reset_rx_done_out                 (gtwiz_reset_rx_done_int)
   ,.gtwiz_userdata_tx_in                    (gtwiz_userdata_tx_int)
   ,.gtwiz_userdata_rx_out                   (gtwiz_userdata_rx_int)
   ,.gtrefclk00_in                           (gtrefclk00_int)
   ,.qpll0outclk_out                         (qpll0outclk_int)
   ,.qpll0outrefclk_out                      (qpll0outrefclk_int)
   ,.loopback_in                             (loopback_int)
   ,.rx8b10ben_in                            (rx8b10ben_int)
   ,.rxbufreset_in                           (rxbufreset_int)
   ,.rxchbonden_in                           (rxchbonden_int)
   ,.rxchbondi_in                            (rxchbondi_int)
   ,.rxchbondlevel_in                        (rxchbondlevel_int)
   ,.rxchbondmaster_in                       (rxchbondmaster_int)
   ,.rxchbondslave_in                        (rxchbondslave_int)
   ,.rxcommadeten_in                         (rxcommadeten_int)
   ,.rxlpmen_in                              (rxlpmen_int)
   ,.rxmcommaalignen_in                      (rxmcommaalignen_int)
   ,.rxpcommaalignen_in                      (rxpcommaalignen_int)
   ,.rxpolarity_in                           (rxpolarity_int)
   ,.tx8b10ben_in                            (tx8b10ben_int)
   ,.txctrl0_in                              (txctrl0_int)
   ,.txctrl1_in                              (txctrl1_int)
   ,.txctrl2_in                              (txctrl2_int)
   ,.txpolarity_in                           (txpolarity_int)
   ,.gtpowergood_out                         (gtpowergood_int)
   ,.rxbufstatus_out                         (rxbufstatus_int)
   ,.rxbyteisaligned_out                     (rxbyteisaligned_int)
   ,.rxbyterealign_out                       (rxbyterealign_int)
   ,.rxchanbondseq_out                       (rxchanbondseq_int)
   ,.rxchanisaligned_out                     (rxchanisaligned_int)
   ,.rxchanrealign_out                       (rxchanrealign_int)
   ,.rxchbondo_out                           (rxchbondo_int)
   ,.rxclkcorcnt_out                         (rxclkcorcnt_int)
   ,.rxcommadet_out                          (rxcommadet_int)
   ,.rxctrl0_out                             (rxctrl0_int)
   ,.rxctrl1_out                             (rxctrl1_int)
   ,.rxctrl2_out                             (rxctrl2_int)
   ,.rxctrl3_out                             (rxctrl3_int)
   ,.rxpmaresetdone_out                      (rxpmaresetdone_int)
   ,.txpmaresetdone_out                      (txpmaresetdone_int)
);


endmodule
