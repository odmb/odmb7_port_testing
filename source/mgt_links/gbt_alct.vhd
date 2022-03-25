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

entity gbt_alct is
  generic (
    NLINK       : integer := 1
    );
  port (
    -- Clocks
    mgtrefclk   : in  std_logic;  --! Input MGT reference clock signal after buffer
    cmsclk      : in  std_logic;  --! Independent clock signal to drive for the helper block of the MGT IP, 80 MHz
    drpclk      : in  std_logic;  --! DRPCLK for the gtwizard module, 120.24 MHz derived from mgtrefclk0_225
    rxusrclk    : out std_logic;  --! The USRCLK signal for RX data readout 120.24 MHz
    rxclken     : out std_logic;  --! The clock enable signal for the RX data readout, 40.079 MHz, 1/3 duty cycle

    -- Serial data ports for transceiver at bank 224-225
    daq_rx_n    : in std_logic; --! Connected to differential optical input signals
    daq_rx_p    : in std_logic; --! Connected to differential optical input signals

    -- Receiver signals
    rxdata      : out std_logic_vector(71 downto 0); --! 4 words
    rxd_valid   : out std_logic; --! Flag for valid data
    bad_rx      : out std_logic; --! Flag for fiber errors
    rxready     : out std_logic; --! Flag for rx reset done
    kill        : in  std_logic; --! Kill signal for ALCT readout

    -- -- Data FIFO full signals
    -- fifo_full   : in std_logic_vector(NLINK downto 1);    --! Flag for ALCT data FIFO full
    -- fifo_afull  : in std_logic_vector(NLINK downto 1);    --! Flag for ALCT data FIFO almost full

    -- Temporary for debug
    rxdata_remap : out std_logic_vector(111 downto 0);

    -- Reset
    reset        : in  std_logic                          --! The Global reset signal
    );
end gbt_alct;

architecture Behavioral of gbt_alct is

  constant NUM_LINKS    : integer := NLINK;

  component gtwiz_alct_r1
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

  signal gthtxp_int                      : std_logic;
  signal gthtxn_int                      : std_logic;
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
  signal mgt_headerflag_s                : std_logic_vector(1 to NUM_LINKS);

  --===========--
  -- GBT Tx/Rx --
  --===========--
  -- signal gbt_txreset_s                   : std_logic_vector(1 to NUM_LINKS);
  -- signal gbt_rxreset_s                   : std_logic_vector(1 to NUM_LINKS);
  -- signal gbt_txisdata_s                  : std_logic_vector(1 to NUM_LINKS);
  -- signal gbt_rxisdata_s                  : std_logic_vector(1 to NUM_LINKS);
  signal gbt_txclken_s                   : std_logic_vector(1 to NUM_LINKS);
  signal gbt_rxclken_s                   : std_logic_vector(1 to NUM_LiNKS);
  signal mgt_txword_s                    : word_mxnbit_A(1 to NUM_LINKS);     --! Tx word to the transceiver (from the Tx gearbox to the MGT)
  signal mgt_rxword_s                    : word_mxnbit_A(1 to NUM_LINKS);     --! Rx word from the transceiver (from the transceiver to the Rx gearbox)

  signal gbt_rxerror_s                   : std_logic_vector(1 to NUM_LINKS);
  signal gbt_rxready_s                   : std_logic_vector(1 to NUM_LINKS);

  --===== GBT Rx Phase Aligner =====--
  signal gbt_rxusrclk      : std_logic; -- User clock for ALCT, at 120 MHz
  signal gbt_rxclken       : std_logic; -- Clock enable signal, on for every third clock of the usrclk
  signal alct_rxdata_gbt   : std_logic_vector(83 downto 0);  -- Data received for GBT Frame
  signal alct_rxdata_wb    : std_logic_vector(31 downto 0);  -- Extra data for Wide Bus
  signal alct_rxdata_raw   : std_logic_vector(111 downto 0);

  signal alct_data_word    : t_std18_array(4 downto 0);
  signal alct_rxdata_pkt   : t_std14_array(7 downto 0);
  signal alct_halfword_reg : std_logic_vector(13 downto 0); -- latched for the last packet

  -- Debugging signals --
  signal ila_data_mgt      : std_logic_vector(83 downto 0);

  --===== Helper function =====--
  function extract_alct_word_from_frame (data  : std_logic_vector(111 downto 0);
                                         index : integer)
    return std_logic_vector is
  begin
    return (data(104 + index) & data(96 + index) & data(88 + index) & data(80 + index) &
            data( 72 + index) & data(64 + index) & data(56 + index) & data(48 + index) &
            data( 40 + index) & data(32 + index) & data(24 + index) & data(16 + index) &
            data(  8 + index) & data( 0 + index));
  end;

begin

  --========================================--
  -- ALCT data handling (subject to change) --
  --========================================--
  RXDATA <= alct_data_word(3) & alct_data_word(2) & alct_data_word(1) & alct_data_word(0);

  -- gbt_txisdata_s(1) <= TXD_VALID;
  alct_rxdata_raw <= alct_rxdata_wb & alct_rxdata_gbt(79 downto 0);

  rxdata_pkt_gen: for i in 0 to 7 generate
    alct_rxdata_pkt(i) <= extract_alct_word_from_frame(alct_rxdata_raw, i);
  end generate;

  alct_word_assemble : process (gbt_rxusrclk, gbt_rxclken)
  begin
    if (rising_edge(gbt_rxusrclk) and gbt_rxclken = '1') then
      if (alct_rxdata_pkt(0)(13) = '1') then
        alct_data_word(0) <= alct_rxdata_pkt(1)(8) & alct_rxdata_pkt(1)(6 downto 0) & alct_rxdata_pkt(0)(9 downto 0);
        alct_data_word(1) <= alct_rxdata_pkt(3)(8) & alct_rxdata_pkt(3)(6 downto 0) & alct_rxdata_pkt(2)(9 downto 0);
        alct_data_word(2) <= alct_rxdata_pkt(5)(8) & alct_rxdata_pkt(5)(6 downto 0) & alct_rxdata_pkt(4)(9 downto 0);
        alct_data_word(3) <= alct_rxdata_pkt(7)(8) & alct_rxdata_pkt(7)(6 downto 0) & alct_rxdata_pkt(6)(9 downto 0);
      else
        alct_data_word(0) <= alct_rxdata_pkt(0)(8) & alct_rxdata_pkt(0)(6 downto 0) & alct_halfword_reg(9 downto 0);
        alct_data_word(1) <= alct_rxdata_pkt(2)(8) & alct_rxdata_pkt(2)(6 downto 0) & alct_rxdata_pkt(1)(9 downto 0);
        alct_data_word(2) <= alct_rxdata_pkt(4)(8) & alct_rxdata_pkt(4)(6 downto 0) & alct_rxdata_pkt(3)(9 downto 0);
        alct_data_word(3) <= alct_rxdata_pkt(6)(8) & alct_rxdata_pkt(6)(6 downto 0) & alct_rxdata_pkt(5)(9 downto 0);
        alct_halfword_reg <= alct_rxdata_pkt(7);
      end if;
    end if;
  end process;

  --====================--
  -- Rx/Tx Status flags --
  --====================--
  RXREADY <= and_reduce(rxready_int);
  BAD_RX <= rxerror_int(0);

  --============--
  -- Clocks     --
  --============--
  RXUSRCLK  <= rxusrclk_int;
  RXCLKEN   <= gbt_rxclken;
  -- TXUSRCLK  <= txusrclk_int; -- for FULL_MGTFREQ clocking scheme
  -- TXCLKEN   <= gbt_txclken_s(1);

  --=============--
  -- GBT Wrapper --
  --=============--
  gbt_wrapper_inst : entity work.gbt_wrapper
    generic map (
      NUM_LINKS                => NLINK,
      TX_ENCODING              => WIDE_BUS,   -- 0: GBT_FRAME, 1: WIDE_BUS, 2: GBT_DYNAMIC
      RX_ENCODING              => WIDE_BUS    -- 0: GBT_FRAME, 1: WIDE_BUS, 2: GBT_DYNAMIC
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
      GBT_TXCLKEN_o            => open,
      GBT_RXCLKEN_o            => gbt_rxclken,

      --==============--
      -- Data          --
      --==============--
      GBT_TXDATA_i(1)          => (others => '0'),
      GBT_RXDATA_o(1)          => alct_rxdata_gbt,

      WB_TXDATA_i(1)           => (others => '0'),
      WB_RXDATA_o(1)           => alct_rxdata_wb,

      TX_ISDATA_i(1)           => '0',
      RX_ISDATA_o(1)           => RXD_VALID,

      MGT_TXWORD_o             => mgt_txword_s,
      MGT_RXWORD_i             => mgt_rxword_s,

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

  mgt_rxreset_done <= gtwiz_reset_rx_done_int and gtwiz_buffbypass_rx_done_int;

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


  gtwiz_alct_inst : gtwiz_alct_r1
    port map (
      gthrxn_in(0)                           => DAQ_RX_N,
      gthrxp_in(0)                           => DAQ_RX_P,
      gthtxn_out(0)                          => gthtxn_int,
      gthtxp_out(0)                          => gthtxp_int,

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

  ---------------------------------------------------------------------------------------------------------------------
  -- Debugging
  ---------------------------------------------------------------------------------------------------------------------
  RXDATA_REMAP <= alct_rxdata_pkt(7) & alct_rxdata_pkt(6) & alct_rxdata_pkt(5) & alct_rxdata_pkt(4) &
                  alct_rxdata_pkt(3) & alct_rxdata_pkt(2) & alct_rxdata_pkt(1) & alct_rxdata_pkt(0);

end Behavioral;
