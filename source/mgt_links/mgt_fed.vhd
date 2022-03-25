--------------------------------------------------------------------------------
-- MGT wrapper
-- Based on example design
--------------------------------------------------------------------------------

library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_misc.all;

library work;
use work.ucsb_types.all;

library UNISIM;
use UNISIM.VComponents.all;

-- Custom libraries and packages:
use work.gbt_bank_package.all;
use work.vendor_specific_gbt_bank_package.all;

entity mgt_fed is
  generic (
    NLINK       : integer := 1
    );
  port (
    -- Clocks
    mgtrefclk   : in  std_logic;  --! Input MGT reference clock signal after buffer
    cmsclk      : in  std_logic;  --! Independent clock signal to drive for the helper block of the MGT IP, 80 MHz
    drpclk      : in  std_logic;  --! DRPCLK for the gtwizard module, 120.24 MHz derived from mgtrefclk0_225
    ilaclk      : in  std_logic;  --! FIXME: temporary used for ILACLK, 120.24 MHz 
    txusrclk    : out std_logic;  --! The USRCLK signal for RX data readout 120.24 MHz
    txclken     : out std_logic;  --! The clock enable signal for the RX data readout, 40.079 MHz, 1/3 duty cycle
    rxusrclk    : out std_logic;  --! The USRCLK signal for RX data readout 120.24 MHz
    rxclken     : out std_logic;  --! The clock enable signal for the RX data readout, 40.079 MHz, 1/3 duty cycle

    -- Serial data ports for transceiver at bank 224-225
    daq_rx_n    : in  std_logic_vector(NLINK downto 1); --! Connected to differential optical input signals
    daq_rx_p    : in  std_logic_vector(NLINK downto 1); --! Connected to differential optical input signals
    daq_tx_n    : out std_logic_vector(NLINK downto 1); --! Connected to differential optical input signals
    daq_tx_p    : out std_logic_vector(NLINK downto 1); --! Connected to differential optical input signals

    -- Tranceiver signals
    txdata      : in  t_std68_array(NLINK downto 1);    --! User TX data, after x"CF00"
    txd_valid   : in  std_logic_vector(NLINK downto 1); --! Flag for valid data
    txready     : out std_logic;                        --! Flag for rx reset done
    rxdata      : out t_std84_array(NLINK downto 1);    --! Full RX gbt data
    rxd_valid   : out std_logic_vector(NLINK downto 1); --! Flag for valid data
    bad_rx      : out std_logic_vector(NLINK downto 1); --! Flag for fiber errors
    rxready     : out std_logic;                        --! Flag for rx reset done

    -- Reset
    reset       : in  std_logic                         --! The Global reset signal
    );
end mgt_fed;

architecture Behavioral of mgt_fed is

  constant NUM_LINKS    : integer := NLINK;

  -- Temporary for debug
  component ila_gbt is
    port (
      clk: in std_logic;
      probe0: in std_logic_vector(111 downto 0);
      probe1: in std_logic_vector(71 downto 0);
      probe2: in std_logic_vector(83 downto 0);
      probe3: in std_logic_vector(0 downto 0)
      );
  end component;

  component gtwiz_fed_d1
    port (
      gtwiz_userclk_tx_active_in : in std_logic_vector(0 downto 0);
      gtwiz_userclk_rx_active_in : in std_logic_vector(0 downto 0);
      gtwiz_buffbypass_tx_reset_in : in std_logic_vector(0 downto 0);
      gtwiz_buffbypass_tx_start_user_in : in std_logic_vector(0 downto 0);
      gtwiz_buffbypass_tx_done_out : out std_logic_vector(0 downto 0);
      gtwiz_buffbypass_tx_error_out : out std_logic_vector(0 downto 0);
      gtwiz_buffbypass_rx_reset_in : in std_logic_vector(0 downto 0);
      gtwiz_buffbypass_rx_start_user_in : in std_logic_vector(0 downto 0);
      gtwiz_buffbypass_rx_done_out : out std_logic_vector(0 downto 0);
      gtwiz_buffbypass_rx_error_out : out std_logic_vector(0 downto 0);
      gtwiz_reset_clk_freerun_in : in std_logic_vector(0 downto 0);
      gtwiz_reset_all_in : in std_logic_vector(0 downto 0);
      gtwiz_reset_tx_pll_and_datapath_in : in std_logic_vector(0 downto 0);
      gtwiz_reset_tx_datapath_in : in std_logic_vector(0 downto 0);
      gtwiz_reset_rx_pll_and_datapath_in : in std_logic_vector(0 downto 0);
      gtwiz_reset_rx_datapath_in : in std_logic_vector(0 downto 0);
      gtwiz_reset_rx_cdr_stable_out : out std_logic_vector(0 downto 0);
      gtwiz_reset_tx_done_out : out std_logic_vector(0 downto 0);
      gtwiz_reset_rx_done_out : out std_logic_vector(0 downto 0);
      gtwiz_userdata_tx_in : in std_logic_vector(39 downto 0);
      gtwiz_userdata_rx_out : out std_logic_vector(39 downto 0);
      drpaddr_in : in std_logic_vector(8 downto 0);
      drpclk_in : in std_logic_vector(0 downto 0);
      drpdi_in : in std_logic_vector(15 downto 0);
      drpen_in : in std_logic_vector(0 downto 0);
      drpwe_in : in std_logic_vector(0 downto 0);
      gthrxn_in : in std_logic_vector(0 downto 0);
      gthrxp_in : in std_logic_vector(0 downto 0);
      gtrefclk0_in : in std_logic_vector(0 downto 0);
      loopback_in : in std_logic_vector(2 downto 0);
      rxpolarity_in : in std_logic_vector(0 downto 0);
      rxslide_in : in std_logic_vector(0 downto 0);
      rxusrclk_in : in std_logic_vector(0 downto 0);
      rxusrclk2_in : in std_logic_vector(0 downto 0);
      txdiffctrl_in : in std_logic_vector(3 downto 0);
      txpolarity_in : in std_logic_vector(0 downto 0);
      txpostcursor_in : in std_logic_vector(4 downto 0);
      txprecursor_in : in std_logic_vector(4 downto 0);
      txusrclk_in : in std_logic_vector(0 downto 0);
      txusrclk2_in : in std_logic_vector(0 downto 0);
      cplllock_out : out std_logic_vector(0 downto 0);
      drpdo_out : out std_logic_vector(15 downto 0);
      drprdy_out : out std_logic_vector(0 downto 0);
      gthtxn_out : out std_logic_vector(0 downto 0);
      gthtxp_out : out std_logic_vector(0 downto 0);
      gtpowergood_out : out std_logic_vector(0 downto 0);
      rxoutclk_out : out std_logic_vector(0 downto 0);
      rxpmaresetdone_out : out std_logic_vector(0 downto 0);
      txoutclk_out : out std_logic_vector(0 downto 0);
      txpmaresetdone_out : out std_logic_vector(0 downto 0)
    );
  end component;

  --======== Naming conventions ======--
  -- suffix _int or without follows (NLINK-1 downto 0) interface to gtwizard
  -- suffix _s or _sig follows (1 to NUM_LINKS) interface to gbt_bank

  -- GT wizard control
  signal rxoutclk_int                    : std_logic_vector(NLINK-1 downto 0);
  signal txoutclk_int                    : std_logic_vector(NLINK-1 downto 0);
  signal rx_usrclk_int                   : std_logic_vector(NLINK-1 downto 0);
  signal tx_usrclk_int                   : std_logic_vector(NLINK-1 downto 0);

  signal rxusrclk_int                    : std_logic;
  signal txusrclk_int                    : std_logic;
  signal mgt_txready                     : std_logic := '0';
  signal mgt_rxready                     : std_logic := '0';
  signal mgt_rxreset_done                : std_logic := '0';
  signal gtwiz_reset_all_int             : std_logic := '0';
  signal gtwiz_userclk_tx_reset_int      : std_logic := '0';
  signal gtwiz_userclk_rx_reset_int      : std_logic := '0';
  signal gtwiz_reset_tx_done_int         : std_logic := '0';
  signal gtwiz_reset_rx_done_int         : std_logic := '0';
  signal gtwiz_userclk_tx_active_int     : std_logic := '0';
  signal gtwiz_userclk_rx_active_int     : std_logic := '0';
  signal gtwiz_buffbypass_tx_reset_int   : std_logic := '0';
  signal gtwiz_buffbypass_rx_reset_int   : std_logic := '0';
  signal gtwiz_buffbypass_tx_done_int    : std_logic := '0';
  signal gtwiz_buffbypass_rx_done_int    : std_logic := '0';
  signal gtwiz_tx_reset_int              : std_logic := '0';
  signal gtwiz_rx_reset_int              : std_logic := '0';
  signal gtwiz_reset_rx_cdr_stable_int   : std_logic := '0';
  signal rxBuffBypassRst                 : std_logic := '0';
  signal drprdy_int                      : std_logic := '0';

  signal gtwiz_userdata_tx_int           : std_logic_vector(40*NLINK-1 downto 0);
  signal gtwiz_userdata_rx_int           : std_logic_vector(40*NLINK-1 downto 0);

  signal rxpmaresetdone_int              : std_logic_vector(NLINK-1 downto 0);
  signal txpmaresetdone_int              : std_logic_vector(NLINK-1 downto 0);
  signal rxslide_int                     : std_logic_vector(NLINK-1 downto 0);
  signal rxerror_int                     : std_logic_vector(NLINK-1 downto 0);
  signal rxready_int                     : std_logic_vector(NLINK-1 downto 0);

  signal rxbitslip_s                     : std_logic_vector(1 to NUM_LINKS);
  signal rxbitslip_done_s                : std_logic_vector(1 to NUM_LINKS);

  --===========--
  -- GBT Tx/Rx --
  --===========--
  signal gbt_rxclken                     : std_logic; -- Clock enable signal, on for every third clock of the usrclk
  signal gbt_txclken                     : std_logic; -- Clock enable signal, on for every third clock of the usrclk

  signal gbt_txclken_s                   : std_logic_vector(1 to NUM_LINKS);
  signal gbt_rxclken_s                   : std_logic_vector(1 to NUM_LiNKS);
  signal mgt_txword_s                    : word_mxnbit_A(1 to NUM_LINKS);     --! Tx word to the transceiver (from the Tx gearbox to the MGT)
  signal mgt_rxword_s                    : word_mxnbit_A(1 to NUM_LINKS);     --! Rx word from the transceiver (from the transceiver to the Rx gearbox)

  signal gbt_rxerror_s                   : std_logic_vector(1 to NUM_LINKS);
  signal gbt_rxready_s                   : std_logic_vector(1 to NUM_LINKS);

  -- Focus only on case when only 1 link is used for GBT
  signal fed_rxdata_gbt    : std_logic_vector(83 downto 0);  -- Data received for GBT Frame
  signal fed_rxdata_wb     : std_logic_vector(31 downto 0);  -- Extra data for Wide Bus
  signal fed_txdata_gbt    : std_logic_vector(83 downto 0);  -- Data received for GBT Frame
  signal fed_txdata_wb     : std_logic_vector(31 downto 0);  -- Extra data for Wide Bus
  signal fed_rxd_valid     : std_logic; -- Flag for valid data

  signal fed_data_word    : t_std18_array(4 downto 0);
  signal fed_rxdata_pkt   : t_std14_array(7 downto 0);
  signal fed_halfword_reg : std_logic_vector(13 downto 0); -- latched for the last packet

  -- Debugging signals --
  signal ila_data_gbt   : std_logic_vector(83 downto 0);

  signal ila_data_0     : std_logic_vector(111 downto 0);
  signal ila_data_1     : std_logic_vector(71 downto 0);
  signal ila_data_2     : std_logic_vector(83 downto 0);


begin

  RXDATA(1) <= fed_rxdata_gbt;
  fed_txdata_gbt <= x"CF00" & TXDATA(1);

  --====================--
  -- Rx/Tx Status flags --
  --====================--
  TXREADY <= mgt_txready;
  RXREADY <= and_reduce(rxready_int);
  BAD_RX <= rxerror_int;

  --============--
  -- Clocks     --
  --============--
  RXUSRCLK <= rxusrclk_int;
  TXUSRCLK <= txusrclk_int; -- for FULL_MGTFREQ clocking scheme
  RXCLKEN <= gbt_rxclken;
  TXCLKEN <= gbt_txclken;

  --=============--
  -- GBT Wrapper --
  --=============--
  gbt_wrapper_inst : entity work.gbt_wrapper
    generic map (
      NUM_LINKS                => NLINK,
      TX_ENCODING              => GBT_FRAME,   -- 0: GBT_FRAME, 1: WIDE_BUS, 2: GBT_DYNAMIC
      RX_ENCODING              => GBT_FRAME    -- 0: GBT_FRAME, 1: WIDE_BUS, 2: GBT_DYNAMIC
      )
    port map (
      --==============--
      -- Clocks       --
      --==============--
      MGT_REFCLK               => MGTREFCLK,
      GBT_CLK                  => CMSCLK,     -- use CMSCLK, 40 MHz
      MGT_DRP_CLK              => DRPCLK,

      GBT_TXUSRCLK_i           => txusrclk_int,
      GBT_RXUSRCLK_i           => rxusrclk_int,
      GBT_TXCLKEN_o            => gbt_txclken,
      GBT_RXCLKEN_o            => gbt_rxclken,

      --==============--
      -- Data          --
      --==============--
      GBT_TXDATA_i(1)          => fed_txdata_gbt,
      GBT_RXDATA_o(1)          => fed_rxdata_gbt,
      WB_TXDATA_i(1)           => fed_txdata_wb,
      WB_RXDATA_o(1)           => fed_rxdata_wb,

      TX_ISDATA_i(1)           => TXD_VALID(1),
      RX_ISDATA_o(1)           => RXD_VALID(1),
      MGT_TXWORD_o             => mgt_txword_s,
      MGT_RXWORD_i             => mgt_rxword_s,

      ILA_DATA_o               => ila_data_gbt,

      --================--
      -- MGT Control    --
      --================--
      RXBITSLIP_o              => rxbitslip_s,
      RXBITSLIP_DONE_o         => rxbitslip_done_s,

      MGT_TXRESET_o            => gtwiz_tx_reset_int,   --! Reset the TX path of the transceiver (Tx PLL is reset with the first link)
      MGT_RXRESET_o            => gtwiz_rx_reset_int,   --! Reset the Rx path of the transceiver
      MGT_RXRESET_DONE_i       => mgt_rxreset_done,     --! Reset and buffbypass reset done for the Rx path 

      --==============--
      -- TX/RX Status --
      --==============--
      MGT_TXREADY_i            => mgt_txready,
      MGT_RXREADY_i            => mgt_rxready,
      GBT_RXREADY_o            => gbt_rxready_s,
      GBT_RXERROR_o            => gbt_rxerror_s,

      --==============--
      -- Reset signal --
      --==============--
      RESET_i                  => RESET
      );


  --===========================--
  -- MGT-GBT signal conversion --
  --===========================--
  gtwiz_gbt_translate : for i in NLINK-1 to 0 generate
    gtwiz_userdata_tx_int(39+40*i downto 40*i) <= mgt_txword_s(i+1);
    mgt_rxword_s(i+1) <= gtwiz_userdata_rx_int(39+40*i downto 40*i);

    rxslide_int(i) <= rxbitslip_s(NLINK-i); 

    rxready_int(i) <= mgt_rxready and gbt_rxready_s(NLINK-i);
    rxerror_int(i) <= gbt_rxerror_s(NLINK-i);
  end generate;


  --=============--
  -- Transceiver --
  --=============--
  gtwiz_userclk_tx_reset_int <= nand_reduce(txpmaresetdone_int);
  gtwiz_userclk_rx_reset_int <= nand_reduce(rxpmaresetdone_int);

  mgt_txready <= gtwiz_reset_tx_done_int and gtwiz_buffbypass_tx_done_int;
  mgt_rxready <= gtwiz_reset_rx_done_int and gtwiz_buffbypass_rx_done_int and and_reduce(rxbitslip_done_s);

  rxBuffBypassRst <= not(gtwiz_userclk_rx_active_int) or not(gtwiz_buffbypass_tx_done_int);

  rxusrclk_int <= rx_usrclk_int(0);
  txusrclk_int <= tx_usrclk_int(0);

  resetDoneSynch_rx: entity work.xlx_ku_mgt_ip_reset_synchronizer
    port map(
      clk_in                => rxusrclk_int,
      rst_in                => rxBuffBypassRst,
      rst_out               => gtwiz_buffbypass_rx_reset_int
      );

  resetSynch_tx: entity work.xlx_ku_mgt_ip_reset_synchronizer
    port map(
      clk_in                => txusrclk_int,
      rst_in                => not(gtwiz_userclk_tx_active_int),
      rst_out               => gtwiz_buffbypass_tx_reset_int
      );

  activetxUsrClk_proc: process(gtwiz_userclk_tx_reset_int, txusrclk_int)
  begin
    if gtwiz_userclk_tx_reset_int = '1' then
      gtwiz_userclk_tx_active_int <= '0';
    elsif rising_edge(txusrclk_int) then
      gtwiz_userclk_tx_active_int <= '1';
    end if;

  end process;

  activerxUsrClk_proc: process(gtwiz_userclk_rx_reset_int, rxusrclk_int)
  begin
    if gtwiz_userclk_rx_reset_int = '1' then
      gtwiz_userclk_rx_active_int <= '0';
    elsif rising_edge(rxusrclk_int) then
      gtwiz_userclk_rx_active_int <= '1';
    end if;

  end process;

  gtwiz_usrclk_gen : for i in NLINK-1 to 0 generate
    rxWordClkBuf_inst: bufg_gt
      port map (
        O                     => rx_usrclk_int(i),
        I                     => rxoutclk_int(i),
        CE                    => not(gtwiz_userclk_rx_reset_int),
        DIV                   => "000",
        CLR                   => '0',
        CLRMASK               => '0',
        CEMASK                => '0'
        );

    txWordClkBuf_inst: bufg_gt
      port map (
        O                     => tx_usrclk_int(i),
        I                     => txoutclk_int(i),
        CE                    => not(gtwiz_userclk_tx_reset_int),
        DIV                   => "000",
        CLR                   => '0',
        CLRMASK               => '0',
        CEMASK                => '0'
        );
  end generate;

  gtwiz_fed_inst : gtwiz_fed_d1
    port map (
      gthrxn_in                              => DAQ_RX_N,
      gthrxp_in                              => DAQ_RX_P,
      gthtxn_out                             => DAQ_TX_N,
      gthtxp_out                             => DAQ_TX_P,

      gtrefclk0_in(0)                        => MGTREFCLK,

      rxusrclk_in                            => rx_usrclk_int,
      rxusrclk2_in                           => rx_usrclk_int,
      txusrclk_in                            => tx_usrclk_int,
      txusrclk2_in                           => tx_usrclk_int,
      rxoutclk_out                           => rxoutclk_int,
      txoutclk_out                           => txoutclk_int,

      gtwiz_userclk_tx_active_in(0)          => gtwiz_userclk_tx_active_int,
      gtwiz_userclk_rx_active_in(0)          => gtwiz_userclk_rx_active_int,

      gtwiz_buffbypass_tx_reset_in(0)        => gtwiz_buffbypass_tx_reset_int,
      gtwiz_buffbypass_tx_start_user_in(0)   => '0',
      gtwiz_buffbypass_tx_done_out(0)        => gtwiz_buffbypass_tx_done_int,
      gtwiz_buffbypass_tx_error_out          => open,

      gtwiz_buffbypass_rx_reset_in(0)        => gtwiz_buffbypass_rx_reset_int,
      gtwiz_buffbypass_rx_start_user_in(0)   => '0',
      gtwiz_buffbypass_rx_done_out(0)        => gtwiz_buffbypass_rx_done_int,
      gtwiz_buffbypass_rx_error_out          => open,

      gtwiz_reset_clk_freerun_in(0)          => DRPCLK, -- drp clock used the freerun clock as well

      gtwiz_reset_all_in(0)                  => '0',

      gtwiz_reset_tx_pll_and_datapath_in(0)  => gtwiz_tx_reset_int,
      gtwiz_reset_tx_datapath_in(0)          => '0',

      gtwiz_reset_rx_pll_and_datapath_in(0)  => '0', -- Same PLL is used for TX and RX !
      gtwiz_reset_rx_datapath_in(0)          => gtwiz_rx_reset_int,
      gtwiz_reset_rx_cdr_stable_out(0)       => gtwiz_reset_rx_cdr_stable_int,

      gtwiz_reset_tx_done_out(0)             => gtwiz_reset_tx_done_int,
      gtwiz_reset_rx_done_out(0)             => gtwiz_reset_rx_done_int,

      gtwiz_userdata_tx_in                   => gtwiz_userdata_tx_int,
      gtwiz_userdata_rx_out                  => gtwiz_userdata_rx_int,

      drpaddr_in                             => "000000000",
      drpclk_in(0)                           => DRPCLK,
      drpdi_in                               => x"0000",
      drpen_in(0)                            => '0',
      drpwe_in(0)                            => '0',
      drpdo_out                              => open,
      drprdy_out(0)                          => drprdy_int,

      loopback_in                            => (others => '0'),
      rxpolarity_in(0)                       => '0',       -- Comment: Not inverted
      txpolarity_in(0)                       => '0',       -- Comment: Not inverted

      rxslide_in                             => rxslide_int,

      txdiffctrl_in                          => "1000",    -- Comment: 807 mVppd
      txpostcursor_in                        => "00000",   -- Comment: 0.00 dB (default)
      txprecursor_in                         => "00000",   -- Comment: 0.00 dB (default)

      cplllock_out                           => open,
      gtpowergood_out                        => open,

      rxpmaresetdone_out                     => rxpmaresetdone_int,
      txpmaresetdone_out                     => txpmaresetdone_int
      );


  ila_data_0(83 downto 0) <= fed_txdata_gbt;
  ila_data_0(84) <= mgt_txready;
  ila_data_0(85) <= mgt_rxready;
  ila_data_0(86) <= gbt_rxready_s(1);
  ila_data_0(87) <= gbt_rxerror_s(1);
  ila_data_0(88) <= gtwiz_tx_reset_int;
  ila_data_0(89) <= gtwiz_rx_reset_int;
  ila_data_0(90) <= gtwiz_reset_tx_done_int;
  ila_data_0(91) <= gtwiz_reset_rx_done_int;
  ila_data_0(92) <= gtwiz_buffbypass_tx_done_int;
  ila_data_0(93) <= txusrclk_int;
  ila_data_0(94) <= rxusrclk_int;
  ila_data_0(95) <= gtwiz_userclk_tx_reset_int;
  ila_data_0(96) <= gtwiz_userclk_rx_reset_int;

  ila_data_1 <= ila_data_gbt(71 downto 0);

  -- ila_data_2(39 downto 0) <= mgt_txword_s(1);
  ila_data_2(39 downto 0)  <= mgt_rxword_s(1);
  ila_data_2(79 downto 40) <= gtwiz_userdata_rx_int;

  ila_fed_rx : ila_gbt
    port map (
      clk => ILACLK,
      probe0 => ila_data_0,
      probe1 => ila_data_1,
      probe2 => ila_data_2,
      probe3(0) => gbt_rxclken
      );


end Behavioral;
