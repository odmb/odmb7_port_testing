library ieee;
use ieee.std_logic_1164.all;

PACKAGE odmb7_components is

  -- components used in odmb_ctrl
  component TRGCNTRL is
    generic (
      NCFEB : integer range 1 to 7 := 5  -- Number of DCFEBS, 7 in the final design
      );  
    port (
      CLK           : in std_logic;
      CLK80         : in std_logic;
      RAW_L1A       : in std_logic;
      RAW_LCT       : in std_logic_vector(NCFEB downto 0);
      CAL_LCT       : in std_logic;
      CAL_L1A       : in std_logic;
      LCT_L1A_DLY   : in std_logic_vector(5 downto 0);
      OTMB_PUSH_DLY : in integer range 0 to 63;
      ALCT_PUSH_DLY : in integer range 0 to 63;
      PUSH_DLY      : in integer range 0 to 63;
      ALCT_DAV      : in std_logic;
      OTMB_DAV      : in std_logic;

      CAL_MODE      : in std_logic;
      KILL          : in std_logic_vector(NCFEB+2 downto 1);
      PEDESTAL      : in std_logic;
      PEDESTAL_OTMB : in std_logic;

      ALCT_DAV_SYNC_OUT : out std_logic;
      OTMB_DAV_SYNC_OUT : out std_logic;

      DCFEB_L1A       : out std_logic;
      DCFEB_L1A_MATCH : out std_logic_vector(NCFEB downto 1);
      FIFO_PUSH       : out std_logic;
      FIFO_L1A_MATCH  : out std_logic_vector(NCFEB+2 downto 0);
      LCT_ERR         : out std_logic
      );
  end component;

  component cafifo is
    generic (
      NFEB        : integer range 1 to 7   := 7;  -- Number of DCFEBS, 7 in the final design
      CAFIFO_SIZE : integer range 1 to 128 := 128  -- Number of CAFIFO words
      );  
    port(

      CSP_FREE_AGENT_PORT_LA_CTRL : inout std_logic_vector(35 downto 0);
      clk                         : in    std_logic;
      dduclk                      : in    std_logic;
      l1acnt_rst                  : in    std_logic;
      bxcnt_rst                   : in    std_logic;

      BC0     : in std_logic;
      CCB_BX0 : in std_logic;
      BXRST   : in std_logic;
      BX_DLY  : in integer range 0 to 4095;
      PUSH_DLY  : in integer range 0 to 63;

      l1a          : in std_logic;
      l1a_match_in : in std_logic_vector(NFEB+2 downto 1);

      pop : in std_logic;

      eof_data    : in std_logic_vector(NFEB+2 downto 1);
      alct_dv     : in std_logic;
      otmb_dv     : in std_logic;
      dcfeb0_dv   : in std_logic;
      dcfeb0_data : in std_logic_vector(15 downto 0);
      dcfeb1_dv   : in std_logic;
      dcfeb1_data : in std_logic_vector(15 downto 0);
      dcfeb2_dv   : in std_logic;
      dcfeb2_data : in std_logic_vector(15 downto 0);
      dcfeb3_dv   : in std_logic;
      dcfeb3_data : in std_logic_vector(15 downto 0);
      dcfeb4_dv   : in std_logic;
      dcfeb4_data : in std_logic_vector(15 downto 0);
      dcfeb5_dv   : in std_logic;
      dcfeb5_data : in std_logic_vector(15 downto 0);
      dcfeb6_dv   : in std_logic;
      dcfeb6_data : in std_logic_vector(15 downto 0);

      cafifo_l1a_match : out std_logic_vector(NFEB+2 downto 1);
      cafifo_l1a_cnt   : out std_logic_vector(23 downto 0);
      cafifo_l1a_dav   : out std_logic_vector(NFEB+2 downto 1);
      cafifo_bx_cnt    : out std_logic_vector(11 downto 0);
      cafifo_lost_pckt : out std_logic_vector(NFEB+2 downto 1);
      cafifo_lone      : out std_logic;

      ext_dcfeb_l1a_cnt7 : out std_logic_vector(23 downto 0);
      dcfeb_l1a_dav7     : out std_logic;

      cafifo_prev_next_l1a_match : out std_logic_vector(15 downto 0);
      cafifo_prev_next_l1a       : out std_logic_vector(15 downto 0);
      control_debug              : in  std_logic_vector(143 downto 0);
      cafifo_debug               : out std_logic_vector(15 downto 0);
      cafifo_wr_addr             : out std_logic_vector(7 downto 0);
      cafifo_rd_addr             : out std_logic_vector(7 downto 0)
      );

  end component;

  CONTROL_FSM_PM : CONTROL_FSM
    generic map(NFEB => NFEB)
    port map(
      CSP_CONTROL_FSM_PORT_LA_CTRL => CSP_CONTROL_FSM_PORT_LA_CTRL,
      CLK                          => dduclk,
      CLKCMS                       => clk40,
      RST                          => l1acnt_rst,
      STATUS                       => status,

-- From DMB_VME
      RDFFNXT => rdffnxt,  -- from MBV (currently assigned as a signal to '0')
      KILL => KILL,
      
-- to GigaBit Link
      DOUT => ddu_data_inner,
      DAV  => ddu_data_valid_inner,

-- to Data FIFOs
      OEFIFO_B  => data_fifo_oe,
      RENFIFO_B => data_fifo_re,

-- from Data FIFOs
      FIFO_HALF_FULL => fifo_half_full,
      FFOR_B         => fifo_empty_b,
      DATAIN         => fifo_out(15 downto 0),
      DATAIN_LAST    => fifo_eof,

-- From JTAGCOM
      JOEF => joef,                     -- from LOADFIFO

-- From CONFREG and GA
      DAQMBID => daqmbid,
      AUTOKILLED_DCFEBS => AUTOKILLED_DCFEBS,

-- FROM SW1
      GIGAEN => LOGICH,

-- TO CAFIFO
      FIFO_POP => cafifo_pop,

-- TO PCFIFO
      EOF => eof,

-- DEBUG
      control_debug => control_debug_full,

-- FROM CAFIFO
      cafifo_l1a_dav   => cafifo_l1a_dav_out,
      cafifo_l1a_match => cafifo_l1a_match_out_inner,
      cafifo_l1a_cnt   => cafifo_l1a_cnt_out,
      cafifo_bx_cnt    => cafifo_bx_cnt_out,
      cafifo_lost_pckt => cafifo_lost_pckt_out,
      cafifo_lone      => cafifo_lone
      );

  -- GTH wizard wrapper for DCFEBs
  component gtwizard_ultrascale_0_example_wrapper
    port (
      gthrxn_in : in std_logic_vector(6 downto 0);
      gthrxp_in : in std_logic_vector(6 downto 0);
      gthtxn_out : out std_logic_vector(6 downto 0);
      gthtxp_out : out std_logic_vector(6 downto 0);
      gtwiz_userclk_tx_reset_in : in std_logic;
      gtwiz_userclk_tx_srcclk_out : out std_logic;
      gtwiz_userclk_tx_usrclk_out : out std_logic;
      gtwiz_userclk_tx_usrclk2_out : out std_logic;
      gtwiz_userclk_tx_active_out : out std_logic;
      gtwiz_userclk_rx_reset_in : in std_logic;
      gtwiz_userclk_rx_srcclk_out : out std_logic;
      gtwiz_userclk_rx_usrclk_out : out std_logic;
      gtwiz_userclk_rx_usrclk2_out : out std_logic;
      gtwiz_userclk_rx_active_out : out std_logic;
      gtwiz_reset_clk_freerun_in : in std_logic;
      gtwiz_reset_all_in : in std_logic;
      gtwiz_reset_tx_pll_and_datapath_in : in std_logic;
      gtwiz_reset_tx_datapath_in : in std_logic;
      gtwiz_reset_rx_pll_and_datapath_in : in std_logic;
      gtwiz_reset_rx_datapath_in : in std_logic;
      gtwiz_reset_rx_cdr_stable_out : out std_logic;
      gtwiz_reset_tx_done_out : out std_logic;
      gtwiz_reset_rx_done_out : out std_logic;
      gtwiz_reset_rxreset_out : out std_logic;
      gtwiz_userdata_tx_in : in std_logic_vector(111 downto 0);
      gtwiz_userdata_rx_out : out std_logic_vector(111 downto 0);
      drpclk_in : in std_logic_vector(6 downto 0);
      gtrefclk0_in : in std_logic_vector(6 downto 0);
      rx8b10ben_in : in std_logic_vector(6 downto 0);
      rxbufreset_in : in std_logic_vector(6 downto 0);
      rxcommadeten_in : in std_logic_vector(6 downto 0);
      rxmcommaalignen_in : in std_logic_vector(6 downto 0);
      rxpcommaalignen_in : in std_logic_vector(6 downto 0);
      tx8b10ben_in : in std_logic_vector(6 downto 0);
      txctrl0_in : in std_logic_vector(111 downto 0);
      txctrl1_in : in std_logic_vector(111 downto 0);
      txctrl2_in : in std_logic_vector(55 downto 0);
      gtpowergood_out : out std_logic_vector(6 downto 0);
      cplllock_out : out std_logic_vector(6 downto 0);
      rxbyteisaligned_out : out std_logic_vector(6 downto 0);
      rxbyterealign_out : out std_logic_vector(6 downto 0);
      rxcommadet_out : out std_logic_vector(6 downto 0);
      rxctrl0_out : out std_logic_vector(111 downto 0);
      rxctrl1_out : out std_logic_vector(111 downto 0);
      rxctrl2_out : out std_logic_vector(55 downto 0);
      rxctrl3_out : out std_logic_vector(55 downto 0);
      rxbufstatus_out : out std_logic_vector(20 downto 0);
      rxclkcorcnt_out : out std_logic_vector(13 downto 0);
      rxpmaresetdone_out : out std_logic_vector(6 downto 0);
      txpmaresetdone_out : out std_logic_vector(6 downto 0)
      );
  end component;

  component rx_frame_proc
    port (
      -- Inputs
      CLK : in std_logic;
      RST : in std_logic;                        -- reset signal from VMEMON
      RXDATA : in std_logic_vector(15 downto 0); -- direct rxdata out from gt wrapper
      RX_IS_K : in std_logic_vector(1 downto 0);
      RXDISPERR : in std_logic_vector(1 downto 0);
      RXNOTINTABLE : in std_logic_vector(1 downto 0);
      -- FIFO (almost) full inputs, triggers error state
      FF_FULL : in std_logic;
      FF_AF : in std_logic;
      -- Client outputs
      FRM_DATA : out std_logic_vector(15 downto 0);
      FRM_DATA_VALID : out std_logic;
      GOOD_CRC : out std_logic;
      CRC_CHK_VLD : out std_logic
      );
  end component;

  -- somehow this gives error during compilation in simulation, in sythesis is
  -- fine
  --COMPONENT vio_cfeb
  --PORT (
  --  clk : IN STD_LOGIC;
  --  probe_in0 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
  --  probe_in1 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
  --  probe_in2 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
  --  probe_in3 : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
  --  probe_in4 : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
  --  probe_in5 : IN STD_LOGIC_VECTOR(6 DOWNTO 0);
  --  probe_in6 : IN STD_LOGIC;
  --  probe_in7 : IN STD_LOGIC;
  --  probe_out0 : OUT STD_LOGIC;
  --  probe_out1 : OUT STD_LOGIC;
  --  probe_out2 : OUT STD_LOGIC;
  --  probe_out3 : OUT STD_LOGIC;
  --  probe_out4 : OUT STD_LOGIC;
  --  probe_out5 : OUT STD_LOGIC_VECTOR(6 DOWNTO 0)
  --);
  --END component;

  component gtwizard_ultrascale_0_example_bit_synchronizer 
  PORT (
    clk_in : IN STD_LOGIC;
    i_in   : IN STD_LOGIC;
    o_out  : out STD_LOGIC 
  );
  END component;

END odmb7_components;
  
