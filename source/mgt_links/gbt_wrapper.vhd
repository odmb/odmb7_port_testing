-- IEEE VHDL standard library:
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;

-- Xilinx devices library:
library unisim;
use unisim.vcomponents.all;

-- Custom libraries and packages:
use work.gbt_bank_package.all;
use work.vendor_specific_gbt_bank_package.all;

--=================================================================================================--
--#######################################   Entity   ##############################################--
--=================================================================================================--

entity gbt_wrapper is
  generic (
    NUM_LINKS                                    : integer := 1;
    TX_ENCODING                                  : integer range 0 to 2 := GBT_FRAME; --! 0: GBT_FRAME, 1: WIDE_BUS, 2: GBT_DYNAMIC
    RX_ENCODING                                  : integer range 0 to 2 := GBT_FRAME  --! 0: GBT_FRAME, 1: WIDE_BUS, 2: GBT_DYNAMIC
    );
  port (

    --==============--
    -- Clocks       --
    --==============--
    MGT_REFCLK                                   : in  std_logic;
    GBT_CLK                                      : in  std_logic; -- use CMSCLK, 40 MHz
    MGT_DRP_CLK                                  : in  std_logic;

    GBT_TXUSRCLK_i                               : in  std_logic;
    GBT_RXUSRCLK_i                               : in  std_logic;
    GBT_TXCLKEN_o                                : out std_logic;
    GBT_RXCLKEN_o                                : out std_logic;

    --==============--
    -- Data          --
    --==============--
    GBT_TXDATA_i                                 : in  gbt_reg84_A(1 to NUM_LINKS);
    GBT_RXDATA_o                                 : out gbt_reg84_A(1 to NUM_LINKS);
    WB_TXDATA_i                                  : in  gbt_reg32_A(1 to NUM_LINKS);
    WB_RXDATA_o                                  : out gbt_reg32_A(1 to NUM_LINKS);
    TX_ISDATA_i                                  : in  std_logic_vector(1 to NUM_LINKS);
    RX_ISDATA_o                                  : out std_logic_vector(1 to NUM_LINKS);
    MGT_TXWORD_o                                 : out word_mxnbit_A(1 to NUM_LINKS);
    MGT_RXWORD_i                                 : in  word_mxnbit_A(1 to NUM_LINKS);

    ILA_DATA_o                                   : out std_logic_vector(83 downto 0);       --! Debug data for ILA pass on to higher level structures

    --================--
    -- MGT Control    --
    --================--
    RXBITSLIP_o                                  : out std_logic_vector(1 to NUM_LINKS);
    RXBITSLIP_DONE_o                             : out std_logic_vector(1 to NUM_LINKS);
    MGT_TXRESET_o                                : out std_logic;    --! Reset the TX path of the transceiver (Tx PLL is reset with the first link)
    MGT_RXRESET_o                                : out std_logic;    --! Reset the Rx path of the transceiver
    MGT_RXRESET_DONE_i                           : in  std_logic;    --! Reset the Rx path of the transceiver

    --==============--
    -- TX/RX Status --
    --==============--
    MGT_TXREADY_i                                : in  std_logic;
    MGT_RXREADY_i                                : in  std_logic;
    GBT_RXREADY_o                                : out std_logic_vector(1 to NUM_LINKS);
    GBT_RXERROR_o                                : out std_logic_vector(1 to NUM_LINKS);

    --==============--
    -- Reset signal --
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
  -- MGT      --
  --==========--
  signal mgt_txwordclk_s                 : std_logic_vector(1 to NUM_LINKS);
  signal mgt_rxwordclk_s                 : std_logic_vector(1 to NUM_LINKS);
  signal mgt_txreset_s                   : std_logic_vector(1 to NUM_LINKS);
  signal mgt_rxreset_s                   : std_logic_vector(1 to NUM_LINKS);
  signal mgt_rxreset_done_s              : std_logic_vector(1 to NUM_LINKS);

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

  signal rxbitslip_reset_tx_s            : std_logic_vector(1 to NUM_LINKS);
  signal rxbitslip_reset_rx_s            : std_logic_vector(1 to NUM_LINKS);
  signal rxbitslip_done_s                : std_logic_vector(1 to NUM_LINKS);

  --=====================================================================================--

  -- Debug --
  signal ila_data_mgt                    : std_logic_vector(83 downto 0);

--=================================================================================================--
begin                 --========####   Architecture Body   ####========--
--=================================================================================================--

  --==================================== User Logic =====================================--

  --============--
  -- Clocks     --
  --============--
  GBT_TXCLKEN_o      <= gbt_txclken_s(1);
  GBT_RXCLKEN_o      <= gbt_rxclkenLogic_s(1); -- necessity to be evaluated
  gbt_rxclken_s      <= mgt_headerflag_s;

  gbtBank_Clk_gen: for i in 1 to NUM_LINKS generate

    -- Generate the TX Clock Enable signal every thrid clock --
    mgt_txwordclk_s(i) <= GBT_TXUSRCLK_i;  -- for FULL_MGTFREQ clocking scheme

    txclken_gen: process(RESET_i, mgt_txwordclk_s(i))
      variable flagCnterV : integer range 0 to GBT_WORD_RATIO;
    begin
      if RESET_i = '1' then
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
    mgt_rxwordclk_s(i) <= GBT_RXUSRCLK_i;  -- for FULL_MGTFREQ clocking scheme
    rx_syncShiftReg(i)(0) <= mgt_headerflag_s(i);
    gbt_rxclkenLogic_s(i) <= rx_syncShiftReg(i)(2);

    -- Timing issue: flip-flop stages (configurable)
    rxSyncShiftReg_gen: for j in 1 to 2 generate
      ssr_flipflop_proc: process(MGT_RXREADY_i, mgt_rxwordclk_s(i))
      begin
        if MGT_RXREADY_i = '0' then
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
  MGT_RXRESET_o <= or_reduce(mgt_rxreset_s) or or_reduce(rxbitslip_reset_rx_s);
  MGT_TXRESET_o <= or_reduce(mgt_txreset_s) or or_reduce(rxbitslip_reset_tx_s);

  gbtBank_rst_gen: for i in 1 to NUM_LINKS generate

    gbtBank_gbtBankRst: entity work.gbt_bank_reset
      generic map (
        INITIAL_DELAY                          => 1 * 40e6   --          * 1s
        )
      port map (
        GBT_CLK_I                              => GBT_CLK,
        TX_FRAMECLK_I                          => mgt_txwordclk_s(i),
        TX_CLKEN_I                             => gbt_txclken_s(i),
        RX_FRAMECLK_I                          => mgt_rxwordclk_s(i),
        RX_CLKEN_I                             => gbt_rxclkenLogic_s(i),
        MGTCLK_I                               => MGT_DRP_CLK,

        --===============--
        -- Resets scheme --
        --===============--
        GENERAL_RESET_I                        => RESET_i,
        TX_RESET_I                             => '0',
        RX_RESET_I                             => '0',

        MGT_TX_RESET_O                         => mgt_txreset_s(i),
        MGT_RX_RESET_O                         => mgt_rxreset_s(i),
        GBT_TX_RESET_O                         => gbt_txreset_s(i),
        GBT_RX_RESET_O                         => gbt_rxreset_s(i),

        MGT_TX_RSTDONE_I                       => MGT_TXREADY_i,
        MGT_RX_RSTDONE_I                       => MGT_RXREADY_i
        );

    mgt_rxreset_done_s(i) <= MGT_RXRESET_DONE_i;

  end generate;

  GBT_RXREADY_o <= gbt_rxready_s;

  --============--
  -- GBT Bank   --
  --============--
  gbt_inst: entity work.gbt_bank
    generic map (
      NUM_LINKS                => NUM_LINKS,
      TX_OPTIMIZATION          => STANDARD,     -- 0: STANDARD, 1: LATENCY_OPTIMIZED
      RX_OPTIMIZATION          => STANDARD,     -- 0: STANDARD, 1: LATENCY_OPTIMIZED
      TX_ENCODING              => TX_ENCODING,  -- 0: GBT_FRAME, 1: WIDE_BUS, 2: GBT_DYNAMIC
      RX_ENCODING              => TX_ENCODING   -- 0: GBT_FRAME, 1: WIDE_BUS, 2: GBT_DYNAMIC
      )
    port map (

      --========--
      -- Resets --
      --========--
      MGT_RXRESET_i            => mgt_rxreset_s,
      MGT_RXRESET_DONE_i       => mgt_rxreset_done_s,
      GBT_TXRESET_i            => gbt_txreset_s,
      GBT_RXRESET_i            => gbt_rxreset_s,


      --========--
      -- Clocks --
      --========--
      MGT_DRP_CLK_i            => MGT_DRP_CLK,
      GBT_TXFRAMECLK_i         => mgt_txwordclk_s,
      GBT_TXCLKEn_i            => gbt_txclken_s,
      GBT_RXFRAMECLK_i         => mgt_rxwordclk_s,
      GBT_RXCLKEn_i            => gbt_rxclken_s,
      MGT_TXWORDCLK_i          => mgt_txwordclk_s,
      MGT_RXWORDCLK_i          => mgt_rxwordclk_s,

      --================--
      -- GBT TX Control --
      --================--
      GBT_ISDATAFLAG_i         => TX_ISDATA_i,
      TX_ENCODING_SEL_i        => gbt_txencoding_s,    --! Select the Tx encoding in dynamic mode ('1': GBT / '0': WideBus)

      --=================--
      -- GBT TX Status   --
      --=================--
      TX_PHALIGNED_o           => open,
      TX_PHCOMPUTED_o          => open,

      --================--
      -- GBT RX Control --
      --================--
      RX_ENCODING_SEL_i        => gbt_rxencoding_s,    --! Select the Rx encoding in dynamic mode ('1': GBT / '0': WideBus)

      --=================--
      -- GBT RX Status   --
      --=================--
      GBT_RXREADY_o            => gbt_rxready_s,
      GBT_ISDATAFLAG_o         => RX_ISDATA_o,
      GBT_ERRORDETECTED_o      => GBT_RXERROR_o,
      GBT_ERRORFLAG_o          => open,

      --================--
      -- MGT Control    --
      --================--
      MGT_RSTONBITSLIPEn_i     => (others => '0'),     --! '1' when RX_OPTIMIZATION = LATENCY_OPTIMIZED
      MGT_RSTONEVEN_i          => (others => '0'),

      RXBITSLIP_o              => RXBITSLIP_o,
      RXBITSLIP_DONE_o         => RXBITSLIP_DONE_o,
      RXBITSLIP_TXRESET_o      => rxbitslip_reset_tx_s,
      RXBITSLIP_RXRESET_o      => rxbitslip_reset_rx_s,

      --=================--
      -- MGT Status      --
      --=================--
      MGT_HEADERFLAG_o         => mgt_headerflag_s,
      MGT_HEADERLOCKED_o       => open,
      MGT_RSTCNT_o             => open,

      ILA_DATA_o               => ila_data_mgt,

      --========--
      -- Data   --
      --========--
      MGT_TXWORD_o             => MGT_TXWORD_o,
      MGT_RXWORD_i             => MGT_RXWORD_i,

      GBT_TXDATA_i             => GBT_TXDATA_i,
      GBT_RXDATA_o             => GBT_RXDATA_o,

      WB_TXDATA_i              => WB_TXDATA_i,
      WB_RXDATA_o              => WB_RXDATA_o

      );

--=====================================================================================--
end gbt_wrapper_inst;
--=================================================================================================--
--#################################################################################################--
--=================================================================================================--
