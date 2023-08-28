-- IEEE VHDL standard library:
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Xilinx devices library:
library unisim;
use unisim.vcomponents.all;

-- Custom libraries and packages:
use work.gbt_bank_package.all;
use work.vendor_specific_gbt_bank_package.all;
-- use work.gbt_exampledesign_package.all;

--=================================================================================================--
--#######################################   Entity   ##############################################--
--=================================================================================================--

entity gbt_wrapper is
  generic (
    NUM_LINKS                                    : integer := 1;
    LINK_TYPE                                    : integer := 0; --! LINK_TYPE: select the proper gtwizard IP, with 0: ALCT, 1: BCK_PRS
    TX_ENCODING                                  : integer range 0 to 2 := GBT_FRAME; --! 0: GBT_FRAME, 1: WIDE_BUS, 2: GBT_DYNAMIC
    RX_ENCODING                                  : integer range 0 to 2 := GBT_FRAME  --! 0: GBT_FRAME, 1: WIDE_BUS, 2: GBT_DYNAMIC
    );
  port (

    --==============--
    -- Clocks       --
    --==============--
    MGT_REFCLK                                   : in  std_logic;
    GBT_FRAMECLK                                 : in  std_logic; -- 40 MHz
    MGT_DRP_CLK                                  : in  std_logic; -- 120.24 MHz,

    GBT_TXUSRCLK_o                               : out std_logic_vector(1 to NUM_LINKS);
    GBT_RXUSRCLK_o                               : out std_logic_vector(1 to NUM_LINKS);
    GBT_TXCLKEN_o                                : out std_logic_vector(1 to NUM_LINKS);
    GBT_RXCLKEN_o                                : out std_logic_vector(1 to NUM_LINKS);

    --==============--
    -- Serial lanes --
    --==============--
    MGT_RX_P                                     : in  std_logic_vector(1 to NUM_LINKS);
    MGT_RX_N                                     : in  std_logic_vector(1 to NUM_LINKS);
    MGT_TX_P                                     : out std_logic_vector(1 to NUM_LINKS);
    MGT_TX_N                                     : out std_logic_vector(1 to NUM_LINKS);

    --==============--
    -- Data          --
    --==============--
    GBT_TXDATA_i                                 : in  gbt_reg84_A(1 to NUM_LINKS);
    GBT_RXDATA_o                                 : out gbt_reg84_A(1 to NUM_LINKS);
    WB_TXDATA_i                                  : in  gbt_reg32_A(1 to NUM_LINKS);
    WB_RXDATA_o                                  : out gbt_reg32_A(1 to NUM_LINKS);

    TXD_VALID_i                                  : in  std_logic_vector(1 to NUM_LINKS);
    RXD_VALID_o                                  : out std_logic_vector(1 to NUM_LINKS);

    --==============--
    -- TX/RX Status --
    --==============--
    MGT_TXREADY_o                                : out std_logic_vector(1 to NUM_LINKS);
    MGT_RXREADY_o                                : out std_logic_vector(1 to NUM_LINKS);
    GBT_TXREADY_o                                : out std_logic_vector(1 to NUM_LINKS);
    GBT_RXREADY_o                                : out std_logic_vector(1 to NUM_LINKS);
    GBT_BAD_RX_o                                 : out std_logic_vector(1 to NUM_LINKS);

    --==============--
    -- Reset        --
    --==============--
    RESET_i                                      : in  std_logic
    );
end gbt_wrapper;

--=================================================================================================--
--####################################   Architecture   ###########################################--
--=================================================================================================--

architecture gbt_wrapper_inst of gbt_wrapper is

  --===========--
  -- Constants --
  --===========--
  constant TX_OPTIMIZATION               : integer range 0 to 1 := STANDARD;
  constant RX_OPTIMIZATION               : integer range 0 to 1 := STANDARD;
  constant CLOCKING_SCHEME               : integer range 0 to 1 := 1; -- 0: BC_CLOCK, 1: FULL_MGTFREQ

  --==========--
  -- NGT      --
  --==========--
  signal mgt_txwordclk_s                 : std_logic_vector(1 to NUM_LINKS);
  signal mgt_rxwordclk_s                 : std_logic_vector(1 to NUM_LINKS);
  signal mgt_txreset_s                   : std_logic_vector(1 to NUM_LINKS);
  signal mgt_rxreset_s                   : std_logic_vector(1 to NUM_LINKS);
  signal mgt_txready_s                   : std_logic_vector(1 to NUM_LINKS);
  signal mgt_rxready_s                   : std_logic_vector(1 to NUM_LINKS);

  signal mgt_headerflag_s                : std_logic_vector(1 to NUM_LINKS);
  signal mgt_devspecific_to_s            : mgtDeviceSpecific_i_R;
  signal mgt_devspecific_from_s          : mgtDeviceSpecific_o_R;
  signal resetOnBitslip_s                : std_logic_vector(1 to NUM_LINKS);

  --===========--
  -- GBT Tx/Rx --
  --===========--
  signal gbt_txreset_s                   : std_logic_vector(1 to NUM_LINKS);
  signal gbt_rxreset_s                   : std_logic_vector(1 to NUM_LINKS);
  signal gbt_txclken_s                   : std_logic_vector(1 to NUM_LINKS);
  signal gbt_rxclken_s                   : std_logic_vector(1 to NUM_LiNKS);
  signal gbt_rxclkenLogic_s              : std_logic_vector(1 to NUM_LiNKS);
  signal gbt_txencoding_s                : std_logic_vector(1 to NUM_LINKS);
  signal gbt_rxencoding_s                : std_logic_vector(1 to NUM_LINKS);
  signal gbt_rxready_s                   : std_logic_vector(1 to NUM_LINKS);
  --===== GBT Rx Phase Aligner =====--
  signal rx_syncShiftReg                 : gbt_devspec_reg3_A(1 to NUM_LINKS);

  --=====================================================================================--

  -- Debug --
  signal ila_data_mgt                    : std_logic_vector(83 downto 0);

  -- component ila_gbt_exde is
  --   port (
  --     clk: in std_logic;
  --     probe0: in std_logic_vector(83 downto 0);
  --     probe1: in std_logic_vector(31 downto 0);
  --     probe2: in std_logic_vector(0 downto 0);
  --     probe3: in std_logic_vector(0 downto 0)
  --     );
  -- end component;

   COMPONENT ila_256 PORT(
     CLK: in std_logic;
     PROBE0: in std_logic_vector(255 downto 0)
   );
   END COMPONENT;
   signal dpr_clk_probe : std_logic_vector(255 downto 0);
   signal gbtbank_rxbitslip_rst_cnt_s : gbt_reg8_A(1 to NUM_LINKS);
   signal rx_clk_probe : std_logic_vector(255 downto 0);
   signal tx_clk_probe : std_logic_vector(255 downto 0);
   signal gbtbank_rx_bitmodified_flag_s : gbt_reg84_A(1 to NUM_LINKS);
   signal gbtbank_rx_errordetected_s : std_logic_vector(1 to NUM_LINKS);
   signal gbt_bad_rx_s : std_logic_vector(1 to NUM_LINKS);
   signal rxd_valid_s : std_logic_vector(1 to NUM_LINKS);
   signal wb_rxdata_s : gbt_reg32_A(1 to NUM_LINKS);
   signal gbt_rxdata_s : gbt_reg84_A(1 to NUM_LINKS);

   COMPONENT vio_gbt
     PORT (
       clk : IN STD_LOGIC;
       probe_out0 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
       probe_out1 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
       probe_out2 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
       probe_out3 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
       probe_out4 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
       probe_out5 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
     );
   END COMPONENT;
   signal gbt_vio_enable     : std_logic := '0';
   signal gbt_reset_from_vio      : std_logic := '0';
   signal gbt_tx_reset_from_vio   : std_logic := '0';
   signal gbt_rx_reset_from_vio   : std_logic := '0';
   signal gbt_txencoding_from_vio : std_logic := '0';
   signal gbt_rxencoding_from_vio : std_logic := '0';
   signal reset_s                 : std_logic := '0';
   signal gbt_tx_reset_s          : std_logic := '0';
   signal gbt_rx_reset_s          : std_logic := '0';

   COMPONENT vio_gbt_tx
     PORT (
       clk : IN STD_LOGIC;
       probe_out0 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
       probe_out1 : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
       probe_out2 : OUT STD_LOGIC_VECTOR(79 DOWNTO 0);
       probe_out3 : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
       probe_out4 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
     );
   END COMPONENT;
   signal gbt_tx_vio_enable       : std_logic := '0';
   signal gbt_tx_valid_from_vio   : std_logic := '0';
   signal tx_scec_from_vio        : std_logic_vector (1 downto 0) := "11";
   signal tx_data_from_vio        : std_logic_vector (79 downto 0);
   signal tx_wb_data_from_vio     : std_logic_vector (31 downto 0);
   signal txd_valid_s             : std_logic_vector (1 to NUM_LINKS) := (others => '0');
   signal gbt_txdata_s            : gbt_reg84_A(1 to NUM_LINKS);
   signal wb_txdata_s             : gbt_reg32_A(1 to NUM_LINKS);

--=================================================================================================--
begin                 --========####   Architecture Body   ####========--
--=================================================================================================--

  --==================================== User Logic =====================================--

  --============--
  -- Clocks     --
  --============--

  GBT_TXUSRCLK_o     <= mgt_txwordclk_s; -- for FULL_MGTFREQ clocking scheme
  GBT_RXUSRCLK_o     <= mgt_rxwordclk_s; -- for FULL_MGTFREQ clocking scheme

  GBT_TXCLKEN_o      <= gbt_txclken_s;
  GBT_RXCLKEN_o      <= gbt_rxclkenLogic_s; -- necessity to be evaluated
  gbt_rxclken_s      <= mgt_headerflag_s;

  gbtBank_Clk_gen: for i in 1 to NUM_LINKS generate

    -- Generate the TX Clock Enable signal every thrid clock --
    txclken_gen: process(reset_s, mgt_txwordclk_s(i))
      variable flagCnterV : integer range 0 to GBT_WORD_RATIO;
    begin
      if reset_s = '1' then
        gbt_txclken_s(i) <= '0';
        flagCnterV := 0;

      elsif rising_edge(mgt_txwordclk_s(i)) then

        flagCnterV := flagCnterV + 1;
        if flagCnterV = GBT_WORD_RATIO then
          flagCnterV := 0;
        end if;

        gbt_txclken_s(i) <= '0';
        if flagCnterV = 0 then
          gbt_txclken_s(i) <= '1';
        end if;

      end if;
    end process;

    -- Generate the RX Clock Enable signal from headerflag --
    rx_syncShiftReg(i)(0) <= mgt_headerflag_s(i);
    gbt_rxclkenLogic_s(i) <= rx_syncShiftReg(i)(2);

    -- Timing issue: flip-flop stages (configurable)
    rxSyncShiftReg_gen: for j in 1 to 2 generate
      ssr_flipflop_proc: process(mgt_rxready_s(i), mgt_rxwordclk_s(i))
      begin
        if mgt_rxready_s(i) = '0' then
          rx_syncShiftReg(i)(j) <= '0';

        elsif rising_edge(mgt_rxwordclk_s(i)) then
          rx_syncShiftReg(i)(j) <= rx_syncShiftReg(i)(j-1);

        end if;
      end process;

    end generate;

  end generate;

  --============--
  -- Resets     --
  --============--
  gbtBank_rst_gen: for i in 1 to NUM_LINKS generate

    gbtBank_gbtBankRst: entity work.gbt_bank_reset
      generic map (
        INITIAL_DELAY                          => 1 * 40e6   --          * 1s
        )
      port map (
        GBT_CLK_I                              => GBT_FRAMECLK,
        TX_FRAMECLK_I                          => mgt_txwordclk_s(i),
        TX_CLKEN_I                             => gbt_txclken_s(i),
        RX_FRAMECLK_I                          => mgt_rxwordclk_s(i),
        RX_CLKEN_I                             => gbt_rxclkenLogic_s(i),
        MGTCLK_I                               => MGT_DRP_CLK,

        --===============--
        -- Resets scheme --
        --===============--
        GENERAL_RESET_I                        => reset_s,
        TX_RESET_I                             => gbt_tx_reset_s,
        RX_RESET_I                             => gbt_rx_reset_s,

        MGT_TX_RESET_O                         => mgt_txreset_s(i),
        MGT_RX_RESET_O                         => mgt_rxreset_s(i),
        GBT_TX_RESET_O                         => gbt_txreset_s(i),
        GBT_RX_RESET_O                         => gbt_rxreset_s(i),

        MGT_TX_RSTDONE_I                       => mgt_txready_s(i),
        MGT_RX_RSTDONE_I                       => mgt_rxready_s(i)
        );

    GBT_TXREADY_o(i) <= not(gbt_txreset_s(i));
  end generate;

  MGT_TXREADY_o <= mgt_txready_s;
  MGT_RXREADY_o <= mgt_rxready_s;
  GBT_RXREADY_o <= gbt_rxready_s;

  --=============--
  -- Transceiver --
  --=============--
  gbtBank_mgt_gen: for i in 1 to NUM_LINKS generate

    mgt_devspecific_to_s.drp_addr(i)               <= "000000000";
    mgt_devspecific_to_s.drp_en(i)                 <= '0';
    mgt_devspecific_to_s.drp_di(i)                 <= x"0000";
    mgt_devspecific_to_s.drp_we(i)                 <= '0';
    mgt_devspecific_to_s.drp_clk(i)                <= MGT_DRP_CLK;

    mgt_devspecific_to_s.prbs_txSel(i)             <= "000";
    mgt_devspecific_to_s.prbs_rxSel(i)             <= "000";
    mgt_devspecific_to_s.prbs_txForceErr(i)        <= '0';
    mgt_devspecific_to_s.prbs_rxCntReset(i)        <= '0';

    mgt_devspecific_to_s.conf_diffCtrl(i)          <= "1000";    -- Comment: 807 mVppd
    mgt_devspecific_to_s.conf_postCursor(i)        <= "00000";   -- Comment: 0.00 dB (default)
    mgt_devspecific_to_s.conf_preCursor(i)         <= "00000";   -- Comment: 0.00 dB (default)
    mgt_devspecific_to_s.conf_txPol(i)             <= '0';       -- Comment: Not inverted
    mgt_devspecific_to_s.conf_rxPol(i)             <= '0';       -- Comment: Not inverted

    mgt_devspecific_to_s.loopBack(i)               <= (others => '0');

    mgt_devspecific_to_s.rx_p(i)                   <= MGT_RX_P(i);
    mgt_devspecific_to_s.rx_n(i)                   <= MGT_RX_N(i);

    mgt_devspecific_to_s.reset_freeRunningClock(i) <= MGT_DRP_CLK;

    MGT_TX_P(i)                                    <= mgt_devspecific_from_s.tx_p(i);
    MGT_TX_N(i)                                    <= mgt_devspecific_from_s.tx_n(i);

    resetOnBitslip_s(i)                            <= '1' when RX_OPTIMIZATION = LATENCY_OPTIMIZED else '0';

  -- gbt_txencoding_s(i)                            <= '1'; -- Not used. Select encoding in dynamic mode ('1': GBT / '0': WideBus)
  -- gbt_rxencoding_s(i)                            <= '1'; -- Not used. Select encoding in dynamic mode ('1': GBT / '0': WideBus)
  end generate;

  --============--
  -- GBT Bank   --
  --============--
  gbt_inst: entity work.gbt_bank
    generic map(
      NUM_LINKS                => NUM_LINKS,
      LINK_TYPE                => LINK_TYPE,
      TX_OPTIMIZATION          => TX_OPTIMIZATION,
      RX_OPTIMIZATION          => RX_OPTIMIZATION,
      TX_ENCODING              => TX_ENCODING,
      RX_ENCODING              => RX_ENCODING
      )
    port map(

      --========--
      -- Resets --
      --========--
      MGT_TXRESET_i            => mgt_txreset_s,
      MGT_RXRESET_i            => mgt_rxreset_s,
      GBT_TXRESET_i            => gbt_txreset_s,
      GBT_RXRESET_i            => gbt_rxreset_s,

      --========--
      -- Clocks --
      --========--
      MGT_CLK_i                => MGT_REFCLK,
      GBT_TXFRAMECLK_i         => mgt_txwordclk_s,
      GBT_TXCLKEn_i            => gbt_txclken_s,
      GBT_RXFRAMECLK_i         => mgt_rxwordclk_s,
      GBT_RXCLKEn_i            => gbt_rxclken_s,
      MGT_TXWORDCLK_o          => mgt_txwordclk_s,
      MGT_RXWORDCLK_o          => mgt_rxwordclk_s,

      --================--
      -- GBT TX Control --
      --================--
      GBT_ISDATAFLAG_i         => txd_valid_s,
      TX_ENCODING_SEL_i        => gbt_txencoding_s,    --! Select the Tx encoding in dynamic mode ('1': GBT / '0': WideBus)

      --=================--
      -- GBT TX Status   --
      --=================--
      TX_PHALIGNED_o          => open,
      TX_PHCOMPUTED_o         => open,

      --================--
      -- GBT RX Control --
      --================--
      RX_ENCODING_SEL_i        => gbt_rxencoding_s,    --! Select the Rx encoding in dynamic mode ('1': GBT / '0': WideBus)

      --=================--
      -- GBT RX Status   --
      --=================--
      GBT_RXREADY_o            => gbt_rxready_s,
      --GBT_ISDATAFLAG_o         => RXD_VALID_o,
      GBT_ISDATAFLAG_o         => rxd_valid_s,
      --GBT_ERRORDETECTED_o      => GBT_BAD_RX_o,
      GBT_ERRORDETECTED_o      => gbt_bad_rx_s,
      GBT_ERRORFLAG_o          => gbtbank_rx_bitmodified_flag_s,

      --================--
      -- MGT Control    --
      --================--
      MGT_DEVSPECIFIC_i        => mgt_devspecific_to_s,
      MGT_RSTONBITSLIPEn_i     => resetOnBitslip_s,
      MGT_RSTONEVEN_i          => (others => '0'),

      --=================--
      -- MGT Status      --
      --=================--
      MGT_TXREADY_o            => mgt_txready_s, --GBTBANK_LINK_TX_READY_O,
      MGT_RXREADY_o            => mgt_rxready_s, --GBTBANK_LINK_RX_READY_O,
      MGT_DEVSPECIFIC_o        => mgt_devspecific_from_s,
      MGT_HEADERFLAG_o         => mgt_headerflag_s,
      MGT_HEADERLOCKED_o       => open,
      MGT_RSTCNT_o             => gbtbank_rxbitslip_rst_cnt_s,

      ILA_DATA_o               => ila_data_mgt,

      --========--
      -- Data   --
      --========--
      GBT_TXDATA_i             => gbt_txdata_s,
      --GBT_RXDATA_o             => GBT_RXDATA_o,
      GBT_RXDATA_o             => gbt_rxdata_s,

      WB_TXDATA_i              => wb_txdata_s,
      --WB_RXDATA_o              => WB_RXDATA_o
      WB_RXDATA_o              => wb_rxdata_s

      );
      GBT_BAD_RX_o <= gbt_bad_rx_s;
      RXD_VALID_o <= rxd_valid_s;
      WB_RXDATA_o <= wb_rxdata_s;
      GBT_RXDATA_o <= gbt_rxdata_s;

  -- ila_gbt_wrapper : ila_gbt_exde
  --   port map (
  --     clk => MGT_DRP_CLK,        -- original 300 MHz
  --     probe0 => ila_data_mgt,
  --     probe1 => (others => '0'),
  --     probe2(0) => gbt_rxclken_s(1),
  --     probe3(0) => gbt_rxclkenLogic_s(1)
  --     );

  --=============--
  -- ILA --
  --=============--
   dpr_clk_ila : ila_256
     port map (
       CLK => MGT_DRP_CLK, -- 120.24 MHz
       PROBE0 => dpr_clk_probe 
     );
   dpr_clk_probe(22) <= mgt_rxready_s(1);
   dpr_clk_probe(21) <= mgt_txready_s(1);
   dpr_clk_probe(20 downto 13) <= gbtbank_rxbitslip_rst_cnt_s(1);
   dpr_clk_probe(8) <= gbt_rxencoding_s(1);
   dpr_clk_probe(7) <= gbt_rxreset_s(1);
   dpr_clk_probe(6) <= mgt_rxreset_s(1);
   dpr_clk_probe(5) <= gbt_txencoding_s(1);
   dpr_clk_probe(4) <= gbt_txreset_s(1);
   dpr_clk_probe(3) <= mgt_txreset_s(1);
   dpr_clk_probe(2) <= gbt_rx_reset_s;
   dpr_clk_probe(1) <= gbt_tx_reset_s;
   dpr_clk_probe(0) <= reset_s;

   rx_clk_ila : ila_256
     port map (
       CLK => mgt_rxwordclk_s(1), -- recovered 120.237 MHz in FULL_MGTREFQ scheme
       PROBE0 => rx_clk_probe 
     );
   rx_clk_probe(205) <= gbt_rxclkenLogic_s(1);
   rx_clk_probe(203 downto 120) <= gbtbank_rx_bitmodified_flag_s(1);
   rx_clk_probe(119) <= gbt_bad_rx_s(1);
   rx_clk_probe(118) <= gbt_rxready_s(1);
   rx_clk_probe(117) <= mgt_headerflag_s(1);
   rx_clk_probe(116) <= rxd_valid_s(1);
   rx_clk_probe(115 downto 84) <= wb_rxdata_s(1);
   rx_clk_probe(83 downto 0) <= gbt_rxdata_s(1);

   tx_clk_ila : ila_256
     port map (
       CLK => mgt_txwordclk_s(1), -- refclk 120.237 MHz in FULL_MGTREFQ scheme
       PROBE0 => tx_clk_probe 
     );
   tx_clk_probe(117) <= gbt_txclken_s(1);
   tx_clk_probe(116) <= txd_valid_s(1);
   tx_clk_probe(115 downto 84) <= wb_txdata_s(1);
   tx_clk_probe(83 downto 0) <= gbt_txdata_s(1);

   -- Setup vio
   vio_gbt_i : vio_gbt
     port map (
       clk        => MGT_DRP_CLK, -- 120.24 MHz,
       probe_out0(0) => gbt_reset_from_vio,
       probe_out1(0) => gbt_tx_reset_from_vio,
       probe_out2(0) => gbt_rx_reset_from_vio,
       probe_out3(0) => gbt_txencoding_from_vio,
       probe_out4(0) => gbt_rxencoding_from_vio,
       probe_out5(0) => gbt_vio_enable
   );
   reset_s <= gbt_reset_from_vio when gbt_vio_enable = '1' else RESET_i;
   gbt_tx_reset_s <= gbt_tx_reset_from_vio when gbt_vio_enable = '1' else '0';
   gbt_rx_reset_s <= gbt_rx_reset_from_vio when gbt_vio_enable = '1' else '0';
   vio_gbt_gen: for i in 1 to NUM_LINKS generate
     gbt_txencoding_s(i) <= gbt_txencoding_from_vio when gbt_vio_enable = '1' else 
                            '0' when TX_ENCODING = WIDE_BUS else
                            '1' when TX_ENCODING = GBT_FRAME; -- Select encoding in dynamic mode ('1': GBT / '0': WideBus)
     gbt_rxencoding_s(i) <= gbt_rxencoding_from_vio when gbt_vio_enable = '1' else 
                            '0' when RX_ENCODING = WIDE_BUS else
                            '1' when RX_ENCODING = GBT_FRAME; -- Select encoding in dynamic mode ('1': GBT / '0': WideBus)
   end generate;

   vio_gbt_tx_i : vio_gbt_tx
     port map (
       clk        => mgt_txwordclk_s(1), -- 120.24 MHz,
       probe_out0(0) => gbt_tx_valid_from_vio,
       probe_out1 => tx_scec_from_vio,
       probe_out2 => tx_data_from_vio,
       probe_out3 => tx_wb_data_from_vio,
       probe_out4(0) => gbt_tx_vio_enable
   );
   vio_gbt_tx_gen: for i in 1 to NUM_LINKS generate
     txd_valid_s(i) <= gbt_tx_valid_from_vio when gbt_tx_vio_enable = '1' else TXD_VALID_i(i);
     -- Comment: The patter is constant "11" in order to reset the SC FSM of the GBTx ASIC.
     gbt_txdata_s(i)(83 downto 82) <= "11"             when gbt_tx_vio_enable = '1' else GBT_TXDATA_i(i)(83 downto 82);
     gbt_txdata_s(i)(81 downto 80) <= tx_scec_from_vio when gbt_tx_vio_enable = '1' else GBT_TXDATA_i(i)(81 downto 80);
     gbt_txdata_s(i)(79 downto  0) <= tx_data_from_vio when gbt_tx_vio_enable = '1' else GBT_TXDATA_i(i)(79 downto  0);
     wb_txdata_s(i)                <= tx_wb_data_from_vio when gbt_tx_vio_enable = '1' else WB_TXDATA_i(i);
   end generate;


--=====================================================================================--
end gbt_wrapper_inst;
--=================================================================================================--
--#################################################################################################--
--=================================================================================================--
