-------------------------------------------------------
--! @file
--! @author Julian Mendez <julian.mendez@cern.ch> (CERN - EP-ESE-BE)
--! @version 6.0
--! @brief GBT-FPGA IP - Device specific transceiver
-------------------------------------------------------

--! IEEE VHDL standard library:
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--! Xilinx devices library:
library unisim;
use unisim.vcomponents.all;

--! Custom libraries and packages:
use work.gbt_bank_package.all;
use work.vendor_specific_gbt_bank_package.all;

--! @brief MGT - Transceiver
--! @details
--! The MGT module provides the interface to the transceivers to send the GBT-links via
--! high speed links (@4.8Gbps)
entity gbt_framealigner is
  generic (
    NUM_LINKS                    : integer := 1
    );
  port (
    --=============--
    -- Clocks      --
    --=============--
    MGT_DRP_CLK_i                : in  std_logic;
    MGT_RXUSRCLK_i               : in  std_logic_vector(1 to NUM_LINKS);

    --=============--
    -- Status      --
    --=============--

    RX_HEADERLOCKED_o            : out std_logic_vector(1 to NUM_LINKS);
    RX_HEADERFLAG_o              : out std_logic_vector(1 to NUM_LINKS);
    MGT_RSTCNT_o                 : out gbt_reg8_A(1 to NUM_LINKS);

    --==============--
    -- Control      --
    --==============--
    MGT_AUTORSTEn_i              : in  std_logic_vector(1 to NUM_LINKS);
    MGT_AUTORSTONEVEN_i          : in  std_logic_vector(1 to NUM_LINKS);

    RXBITSLIP_o                  : out std_logic_vector(1 to NUM_LINKS);
    RXBITSLIP_DONE_o             : out std_logic_vector(1 to NUM_LINKS);
    RXBITSLIP_TXRESET_o          : out std_logic_vector(1 to NUM_LINKS);
    RXBITSLIP_RXRESET_o          : out std_logic_vector(1 to NUM_LINKS);

    MGT_RXRESET_i                : in  std_logic_vector(1 to NUM_LINKS);
    MGT_RXRESET_DONE_i           : in  std_logic_vector(1 to NUM_LINKS);

    --==============--
    -- Data         --
    --==============--
    MGT_USRWORD_i                : in  word_mxnbit_A(1 to NUM_LINKS);

    ILA_DATA_o                   : out std_logic_vector(71 downto 0)
    );
end gbt_framealigner;

--! @brief MGT - Transceiver
--! @details The MGT module implements all the logic required to send the GBT frame on high speed
--! links: resets modules for the transceiver, Tx PLL and alignement logic to align the received word with the
--! GBT frame header.
architecture structural of gbt_framealigner is
  --================================ Signal Declarations ================================--

  --==============================--
  -- RX phase alignment (bitslip) --
  --==============================--
  signal rx_usrclk_sig                    : std_logic_vector(1 to NUM_LINKS);

  signal rxBitSlip_to_gtx                 : std_logic_vector(1 to NUM_LINKS);
  signal done_from_rxBitSlipControl       : std_logic_vector(1 to NUM_LINKS);

  type rstBitSlip_FSM_t                   is (idle, reset_tx, reset_rx);
  type rstBitSlip_FSM_t_A                 is array (natural range <>) of rstBitSlip_FSM_t;
  signal rstBitSlip_FSM                   : rstBitSlip_FSM_t_A(1 to NUM_LINKS);

  signal mgtRst_from_bitslipCtrl          : std_logic_vector(1 to NUM_LINKS);

  signal resetGtxRx_from_rxBitSlipControl : std_logic_vector(1 to NUM_LINKS);
  signal resetGtxTx_from_rxBitSlipControl : std_logic_vector(1 to NUM_LINKS);

  signal bitSlipCmd_to_bitSlipCtrller     : std_logic_vector(1 to NUM_LINKS);
  signal ready_from_bitSlipCtrller        : std_logic_vector(1 to NUM_LINKS);

  signal rx_headerlocked_s                : std_logic_vector(1 to NUM_LINKS);
  signal rx_bitslipIsEven_s               : std_logic_vector(1 to NUM_LINKS);

  signal pattSearch_reset_s               : std_logic_vector(1 to NUM_LINKS);


  signal ila_data_patser : std_logic_vector(7 downto 0);

--=================================================================================================--
begin                 --========####   Architecture Body   ####========--
--=================================================================================================--

  --==================================== User Logic =====================================--

  --=============--
  -- Assignments --
  --=============--
  rx_usrclk_sig <= MGT_RXUSRCLK_i;
  RXBITSLIP_RXRESET_o <= resetGtxRx_from_rxBitSlipControl;
  RXBITSLIP_TXRESET_o <= resetGtxTx_from_rxBitSlipControl;

  RXBITSLIP_o <= rxBitSlip_to_gtx;
  RXBITSLIP_DONE_o <= done_from_rxBitSlipControl;

  gtxLatOpt_gen: for i in 1 to NUM_LINKS generate
    --====================--
    -- RX phase alignment --
    --====================--
    -- Reset on bitslip control module:
    -----------------------------------
    bitslipResetFSM_proc: PROCESS(MGT_DRP_CLK_i, MGT_RXRESET_i(i))
      variable timer :integer range 0 to GBTRX_BITSLIP_MGT_RX_RESET_DELAY;
      variable rstcnt: unsigned(7 downto 0);
    begin

      if MGT_RXRESET_i(i) = '1' then
        resetGtxRx_from_rxBitSlipControl(i) <= '0';
        resetGtxTx_from_rxBitSlipControl(i) <= '0';
        timer  := 0;
        rstcnt := (others => '0');
        rstBitSlip_FSM(i) <= idle;

      elsif rising_edge(MGT_DRP_CLK_i) then

        case rstBitSlip_FSM(i) is
          when idle      => resetGtxRx_from_rxBitSlipControl(i)     <= '0';
                            resetGtxTx_from_rxBitSlipControl(i)     <= '0';

                            if mgtRst_from_bitslipCtrl(i) = '1' then

                              resetGtxRx_from_rxBitSlipControl(i) <= '1';
                              resetGtxTx_from_rxBitSlipControl(i) <= '1';
                              rstBitSlip_FSM(i) <= reset_tx;
                              timer := 0;

                              rstcnt := rstcnt+1;

                            end if;

          when reset_tx  => if timer = GBTRX_BITSLIP_MGT_RX_RESET_DELAY-1 then
                              resetGtxTx_from_rxBitSlipControl(i)     <= '0';
                              rstBitSlip_FSM(i)                       <= reset_rx;
                              timer                                   := 0;
                            else
                              timer := timer + 1;
                            end if;

          when reset_rx  => if timer = GBTRX_BITSLIP_MGT_RX_RESET_DELAY-1 then
                              resetGtxRx_from_rxBitSlipControl(i)     <= '0';
                              rstBitSlip_FSM(i)                       <= idle;
                              timer                                   := 0;
                            else
                              timer := timer + 1;
                            end if;

        end case;

        MGT_RSTCNT_o(i)   <= std_logic_vector(rstcnt);
      end if;

    end process;

    rxBitSlipControl: entity work.mgt_bitslipctrl
      port map (
        RX_RESET_I          => pattSearch_reset_s(i),
        RX_WORDCLK_I        => rx_usrclk_sig(i),
        MGT_CLK_I           => MGT_DRP_CLK_i,

        RX_BITSLIPCMD_i     => bitSlipCmd_to_bitSlipCtrller(i),
        RX_BITSLIPCMD_o     => rxBitSlip_to_gtx(i),

        RX_HEADERLOCKED_i   => rx_headerlocked_s(i),
        RX_BITSLIPISEVEN_i  => rx_bitslipIsEven_s(i),
        RX_RSTONBITSLIP_o   => mgtRst_from_bitslipCtrl(i),
        RX_ENRST_i          => MGT_AUTORSTEn_i(i),
        RX_RSTONEVEN_i      => MGT_AUTORSTONEVEN_i(i),

        DONE_o              => done_from_rxBitSlipControl(i),
        READY_o             => ready_from_bitSlipCtrller(i)
        );

    patternSearch: entity work.mgt_framealigner_pattsearch
      port map (
        RX_RESET_I          => pattSearch_reset_s(i),
        RX_WORDCLK_I        => rx_usrclk_sig(i),

        RX_BITSLIP_CMD_O    => bitSlipCmd_to_bitSlipCtrller(i),
        MGT_BITSLIPDONE_i   => ready_from_bitSlipCtrller(i),

        RX_HEADER_LOCKED_O  => rx_headerlocked_s(i),
        RX_HEADER_FLAG_O    => RX_HEADERFLAG_o(i),
        RX_BITSLIPISEVEN_o  => rx_bitslipIsEven_s(i),
        ILA_DATA_o          => ila_data_patser,

        RX_WORD_I           => MGT_USRWORD_i(i)
        );

    pattSearch_reset_s(i) <= not(MGT_RXRESET_DONE_i(i));
    RX_HEADERLOCKED_o(i) <= rx_headerlocked_s(i);

  end generate;

  -- For debugging the rxclk problem
  -- ILA_DATA_o(0)  <= tx_reset_done(1);
  -- ILA_DATA_o(1)  <= txfsm_reset_done(1);
  -- ILA_DATA_o(2)  <= gtwiz_userclk_tx_active_int(1);
  -- ILA_DATA_o(3)  <= gtwiz_buffbypass_tx_reset_in_s(1);
  -- ILA_DATA_o(4)  <= MGT_TXRESET_i(1); 
  -- ILA_DATA_o(5)  <= gtwiz_userclk_tx_reset_int(1);
  ILA_DATA_o(6)  <= resetGtxTx_from_rxBitSlipControl(1);
  ILA_DATA_o(7)  <= MGT_RXRESET_DONE_i(1);
  -- ILA_DATA_o(9)  <= gtwiz_userclk_rx_active_int(1);
  -- ILA_DATA_o(10) <= gtwiz_buffbypass_rx_reset_in_s(1);
  ILA_DATA_o(11) <= MGT_RXRESET_i(1); 
  -- ILA_DATA_o(12) <= gtwiz_userclk_rx_reset_int(1);
  ILA_DATA_o(13) <= resetGtxRx_from_rxBitSlipControl(1);
  ILA_DATA_o(14) <= done_from_rxBitSlipControl(1);
  ILA_DATA_o(15) <= ready_from_bitSlipCtrller(1);
  ILA_DATA_o(16) <= rxBitSlip_to_gtx(1);
  ILA_DATA_o(17) <= mgtRst_from_bitslipCtrl(1);
  ILA_DATA_o(18) <= MGT_AUTORSTEn_i(1);
  ILA_DATA_o(19) <= MGT_AUTORSTONEVEN_i(1);
  ILA_DATA_o(20) <= rx_headerlocked_s(1);
  ILA_DATA_o(21) <= rx_bitslipIsEven_s(1);
  ILA_DATA_o(61 downto 22) <= MGT_USRWORD_i(1);
  ILA_DATA_o(69 downto 62) <= ila_data_patser;

end structural;
--=================================================================================================--
--#################################################################################################--
--=================================================================================================--
