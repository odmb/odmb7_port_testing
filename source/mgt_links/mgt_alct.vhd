-- MO: ODMB7 ALCT interface based on Kintex Ultrascale GBT example design by Manoel Barros Marin
-- Currently being set up for ALCT-LX100 (ODMB7) with 1 GBT link
-- Will need changes/a different wrapper for ALCT-LX150 (ODMB5) with 2 8B/10B links

-- IEEE VHDL standard library:
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;

-- Xilinx devices library:
library unisim;
use unisim.vcomponents.all;

-- Custom libraries and packages:
library work;
use work.ucsb_types.all;
use work.gbt_bank_package.all;
use work.vendor_specific_gbt_bank_package.all;
use work.gbt_exampledesign_package.all;

--=================================================================================================--
--#######################################   Entity   ##############################################--
--=================================================================================================--

entity mgt_alct is
   generic (
     NUM_LINKS                                              : integer := 1
   );
   port ( 

        --==============--
        -- Clocks       --
        --==============--
        MGTREFCLK_BUF     : in std_logic;                         --MGT clock after buffering for logic
        --FRAMECLK_40MHZ    : in  std_logic;                        
        XCVRCLK           : in  std_logic;                        --MGT refclk
        RX_FRAMECLK_O     : out std_logic_vector(1 to NUM_LINKS); 
        RX_WORDCLK_O      : out std_logic_vector(1 to NUM_LINKS);
        TX_FRAMECLK_O     : out std_logic_vector(1 to NUM_LINKS);
        TX_WORDCLK_O      : out std_logic_vector(1 to NUM_LINKS);
        
        RX_FRAMECLK_RDY_O : out std_logic_vector(1 to NUM_LINKS);
        
        --==============--
        -- Reset        --
        --==============--
        --GBTBANK_GENERAL_RESET_I   : in  std_logic;
        --GBTBANK_MANUAL_RESET_TX_I : in  std_logic;
        --GBTBANK_MANUAL_RESET_RX_I : in  std_logic;
        --TODO: link these to internal vio
        
        --==============--
        -- Serial lanes --
        --==============--
        GBTBANK_MGT_RX_P : in  std_logic_vector(1 to NUM_LINKS);
        GBTBANK_MGT_RX_N : in  std_logic_vector(1 to NUM_LINKS);
        GBTBANK_MGT_TX_P : out std_logic_vector(1 to NUM_LINKS);
        GBTBANK_MGT_TX_N : out std_logic_vector(1 to NUM_LINKS);
        
        --==============--
        -- Data         --
        --==============--        
        GBTBANK_GBT_DATA_I : in  gbt_reg84_A(1 to NUM_LINKS);
        GBTBANK_WB_DATA_I  : in  gbt_reg116_A(1 to NUM_LINKS);
        TX_DATA_O          : out gbt_reg84_A(1 to NUM_LINKS);  -- std_logic_vector(83 downto 0)
        WB_DATA_O          : out gbt_reg116_A(1 to NUM_LINKS); -- std_logic_vector(115 downto 0)
        GBTBANK_GBT_DATA_O : out gbt_reg84_A(1 to NUM_LINKS);  -- this is RX data
        GBTBANK_WB_DATA_O  : out gbt_reg116_A(1 to NUM_LINKS);
        
        --==============--
        -- Reconf.         --
        --==============--
        GBTBANK_MGT_DRP_RST : in  std_logic;
        GBTBANK_MGT_DRP_CLK : in  std_logic; --user/DRP clock
        
        --==============--
        -- TX ctrl        --
        --==============--
        --TX_ENCODING_SEL_i          : in  std_logic_vector(1 to NUM_LINKS);    --! Select the Tx encoding in dynamic mode ('1': GBT / '0': WideBus)
        --GBTBANK_TX_ISDATA_SEL_I    : in  std_logic_vector(1 to NUM_LINKS);
        
        --==============--
        -- RX ctrl      --
        --==============--
        --RX_ENCODING_SEL_i                    : in  std_logic_vector(1 to NUM_LINKS);    --! Select the Rx encoding in dynamic mode ('1': GBT / '0': WideBus)
        --GBTBANK_RESET_GBTRXREADY_LOST_FLAG_I : in  std_logic_vector(1 to NUM_LINKS);             
        --GBTBANK_RESET_DATA_ERRORSEEN_FLAG_I  : in  std_logic_vector(1 to NUM_LINKS);                               
        --GBTBANK_RXFRAMECLK_ALIGNPATTER_I     : in std_logic_vector(2 downto 0);    
        --GBTBANK_RXBITSLIT_RSTONEVEN_I        : in std_logic_vector(1 to NUM_LINKS);
        --TODO: move to VIO
        
        --==============--
        -- TX Status    --
        --==============--
        --GBTBANK_GBTTX_READY_O      : out std_logic_vector(1 to NUM_LINKS);
        --GBTBANK_GBTRX_READY_O      : out std_logic_vector(1 to NUM_LINKS);
        --GBTBANK_LINK_READY_O       : out std_logic_vector(1 to NUM_LINKS);
        --GBTBANK_TX_MATCHFLAG_O     : out std_logic;
        --GBTBANK_TX_ALIGNED_O       : out std_logic_vector(1 to NUM_LINKS);
        --GBTBANK_TX_ALIGNCOMPUTED_O : out std_logic_vector(1 to NUM_LINKS);
        
        --==============--
        -- RX Status    --
        --==============--
        --GBTBANK_GBTRXREADY_LOST_FLAG_O               : out std_logic_vector(1 to NUM_LINKS);
        --GBTBANK_RXDATA_ERRORSEEN_FLAG_O              : out std_logic_vector(1 to NUM_LINKS);
        --GBTBANK_RXEXTRADATA_WIDEBUS_ERRORSEEN_FLAG_O : out std_logic_vector(1 to NUM_LINKS);
        --GBTBANK_RX_MATCHFLAG_O                       : out std_logic_vector(1 to NUM_LINKS);
        --GBTBANK_RX_ISDATA_SEL_O                      : out std_logic_vector(1 to NUM_LINKS);
        --GBTBANK_RX_ERRORDETECTED_O                   : out std_logic_vector(1 to NUM_LINKS);
        GBTBANK_RX_BITMODIFIED_FLAG_O                : out gbt_reg84_A(1 to NUM_LINKS);
        GBTBANK_RXBITSLIP_RST_CNT_O                  : out gbt_reg8_A(1 to NUM_LINKS);
        
        --==============--
        -- XCVR ctrl    --
        --==============--
        --GBTBANK_LOOPBACK_I : in  std_logic_vector(2 downto 0); --move to VIO
        GBTBANK_TX_POL     : in  std_logic_vector(1 to NUM_LINKS);
        GBTBANK_RX_POL     : in  std_logic_vector(1 to NUM_LINKS)
                
   );
end mgt_alct;

--=================================================================================================--
--####################################   Architecture   ###########################################-- 
--=================================================================================================--

architecture structural of mgt_alct is  

    --From Generics
    constant TX_OPTIMIZATION : integer range 0 to 1 := STANDARD;
    constant RX_OPTIMIZATION : integer range 0 to 1 := STANDARD;
    constant TX_ENCODING     : integer range 0 to 2 := GBT_FRAME;
    constant RX_ENCODING     : integer range 0 to 2 := GBT_FRAME;
    constant CLOCKING_SCHEME : integer range 0 to 1 := BC_CLOCK;
   
   --================================ Signal Declarations ================================--   
    --==========--
    -- GBT Tx   --
    --==========--
    signal gbt_txframeclk_s                : std_logic_vector(1 to NUM_LINKS);
    signal gbt_txreset_s                   : std_logic_vector(1 to NUM_LINKS);
    signal gbt_txdata_s                    : gbt_reg84_A(1 to NUM_LINKS);
    signal wb_txdata_s                     : gbt_reg32_A(1 to NUM_LINKS);
    signal gbt_txclken_s                   : std_logic_vector(1 to NUM_LINKS);
    
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
    
    --==========--
    -- GBT Rx   --
    --==========--
    signal gbt_rxframeclk_s                : std_logic_vector(1 to NUM_LINKS);
    signal gbt_rxreset_s                   : std_logic_vector(1 to NUM_LINKS);
    signal gbt_rxready_s                   : std_logic_vector(1 to NUM_LINKS);
    signal gbt_rxdata_s                    : gbt_reg84_A(1 to NUM_LINKS);
    signal wb_rxdata_s                     : gbt_reg32_A(1 to NUM_LINKS);
    signal gbt_rxclken_s                    : std_logic_vector(1 to NUM_LiNKS);
    signal gbt_rxclkenLogic_s               : std_logic_vector(1 to NUM_LiNKS);
    
   --================================--
   -- Data pattern generator/checker --
   --================================--
   signal gbtBank_txEncodingSel            : std_logic_vector(1 downto 0);
   signal gbtBank_rxEncodingSel            : std_logic_vector(1 downto 0);
   signal txData_from_gbtBank_pattGen      : gbt_reg84_A(1 to NUM_LINKS);
   signal txwBData_from_gbtBank_pattGen    : gbt_reg32_A(1 to NUM_LINKS);

   --MO New signals, to be sorted
   signal FRAMECLK_40MHz                       : std_logic;
   --vio outputs
   signal shiftTxClock_from_vio                : std_logic;
   signal txShiftCount_from_vio                : std_logic_vector(7 downto 0);
   signal txPllReset                           : std_logic;
   signal TX_ENCODING_SEL_i                    : std_logic_vector(1 to NUM_LINKS);    --! Select the Tx encoding in dynamic mode ('1': GBT / '0': WideBus)
   signal GBTBANK_TX_ISDATA_SEL_I              : std_logic_vector(1 to NUM_LINKS);
   signal RX_ENCODING_SEL_i                    : std_logic_vector(1 to NUM_LINKS);    --! Select the Rx encoding in dynamic mode ('1': GBT / '0': WideBus)
   signal GBTBANK_RESET_GBTRXREADY_LOST_FLAG_I : std_logic_vector(1 to NUM_LINKS);             
   signal GBTBANK_RESET_DATA_ERRORSEEN_FLAG_I  : std_logic_vector(1 to NUM_LINKS);             
   signal GBTBANK_RXFRAMECLK_ALIGNPATTER_I     : std_logic_vector(2 downto 0);    
   signal GBTBANK_RXBITSLIT_RSTONEVEN_I        : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_LOOPBACK_I                   : std_logic_vector(2 downto 0);
   signal GBTBANK_GENERAL_RESET_I              : std_logic;
   signal GBTBANK_MANUAL_RESET_TX_I            : std_logic;
   signal GBTBANK_MANUAL_RESET_RX_I            : std_logic;

   --vio inputs
   signal GBTBANK_GBTTX_READY_O                : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_GBTRX_READY_O                : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_LINK_READY_O                 : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_TX_MATCHFLAG_O               : std_logic;
   signal GBTBANK_TX_ALIGNED_O                 : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_TX_ALIGNCOMPUTED_O           : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_GBTRXREADY_LOST_FLAG_O               : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_RXDATA_ERRORSEEN_FLAG_O              : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_RXEXTRADATA_WIDEBUS_ERRORSEEN_FLAG_O : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_RX_MATCHFLAG_O                       : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_RX_ISDATA_SEL_O                      : std_logic_vector(1 to NUM_LINKS);
   signal GBTBANK_RX_ERRORDETECTED_O                   : std_logic_vector(1 to NUM_LINKS);
   signal txFrameClkPllLocked_from_gbtExmplDsgn        : std_logic;
   
   COMPONENT xlx_ku_vio PORT (
         clk : IN STD_LOGIC;

         probe_in0 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in1 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in2 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in3 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in4 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in5 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in6 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in7 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in8 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in9 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in10 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in11 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_in12 : IN STD_LOGIC_VECTOR(0 DOWNTO 0);

         probe_out0 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out1 : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
         probe_out2 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out3 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out4 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out5 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out6 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out7 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out8 : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
         probe_out9 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out10 : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
         probe_out11 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out12 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out13 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out14 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0);
         probe_out15 : OUT STD_LOGIC_VECTOR(0 DOWNTO 0)
       );
   END COMPONENT;

   COMPONENT xlx_ku_ila PORT(
        CLK: in std_logic;
        PROBE0: in std_logic_vector(83 downto 0);
        PROBE1: in std_logic_vector(31 downto 0);
        PROBE2: in std_logic_vector(0 downto 0)
     );
   END COMPONENT;
    
   --=====================================================================================--      

--=================================================================================================--
begin                 --========####   Architecture Body   ####========-- 
--=================================================================================================--
   
   --==================================== User Logic =====================================--

    --============--
    -- VIO & ILAs --
    --============--              
    alct_vio : xlx_ku_vio
       PORT MAP (
         clk => GBTBANK_MGT_DRP_CLK,

         probe_in0 => GBTBANK_GBTTX_READY_O,
         probe_in1 => GBTBANK_GBTRX_READY_O,
         probe_in2 => GBTBANK_LINK_READY_O,
         probe_in3(0) => GBTBANK_TX_MATCHFLAG_O,
         probe_in4 => GBTBANK_TX_ALIGNED_O,
         probe_in5 => GBTBANK_TX_ALIGNCOMPUTED_O,
         probe_in6 => GBTBANK_GBTRXREADY_LOST_FLAG_O,
         probe_in7 => GBTBANK_RXDATA_ERRORSEEN_FLAG_O,
         probe_in8 => GBTBANK_RXEXTRADATA_WIDEBUS_ERRORSEEN_FLAG_O,
         probe_in9 => GBTBANK_RX_MATCHFLAG_O,
         probe_in10 => GBTBANK_RX_ISDATA_SEL_O,
         probe_in11 => GBTBANK_RX_ERRORDETECTED_O,
         probe_in12(0) => txFrameClkPllLocked_from_gbtExmplDsgn,

         probe_out0(0) => shiftTxClock_from_vio,
         probe_out1 => txShiftCount_from_vio,
         probe_out2(0) => txPllReset,
         probe_out3 => TX_ENCODING_SEL_i,
         probe_out4 => GBTBANK_TX_ISDATA_SEL_I,
         probe_out5 => RX_ENCODING_SEL_i,
         probe_out6 => GBTBANK_RESET_GBTRXREADY_LOST_FLAG_I,
         probe_out7 => GBTBANK_RESET_DATA_ERRORSEEN_FLAG_I,
         probe_out8 => GBTBANK_RXFRAMECLK_ALIGNPATTER_I,
         probe_out9 => GBTBANK_RXBITSLIT_RSTONEVEN_I,
         probe_out10 => GBTBANK_LOOPBACK_I,
         probe_out11(0) => GBTBANK_GENERAL_RESET_I,
         probe_out12(0) => GBTBANK_MANUAL_RESET_TX_I,
         probe_out13(0) => GBTBANK_MANUAL_RESET_RX_I
       );

    alct_ila : xlx_ku_ila
      PORT MAP (
        clk => gbt_rxframeclk_s(1),
        PROBE0 => gbt_rxdata_s(1),
        PROBE1 => wb_rxdata_s(1),
        PROBE2 => GBTBANK_RX_ISDATA_SEL_O
      );

    --============--
    -- Clocks     --
    --============--              
    --MO move this in from top level in exdes
    txFrameclkGen_inst: entity work.xlx_ku_tx_phaligner
        Port map( 
            -- Reset
            RESET_IN              => txPllReset,
            -- Clocks
            CLK_IN                => MGTREFCLK_BUF,
            CLK_OUT               => FRAMECLK_40MHZ,
            -- Control
            SHIFT_IN              => shiftTxClock_from_vio,
            SHIFT_COUNT_IN        => txShiftCount_from_vio,
            -- Status
            LOCKED_OUT            => txFrameClkPllLocked_from_gbtExmplDsgn
        );
  
    gbtBank_Clk_gen: for i in 1 to NUM_LINKS generate
    
        gbtBank_rxFrmClkPhAlgnr: entity work.gbt_rx_frameclk_phalgnr
            generic map(
                TX_OPTIMIZATION                           => TX_OPTIMIZATION,
                RX_OPTIMIZATION                           => RX_OPTIMIZATION,
                DIV_SIZE_CONFIG                           => 3,
                METHOD                                    => GATED_CLOCK,
                CLOCKING_SCHEME                           => CLOCKING_SCHEME
            )
            port map (            
                RESET_I                                   => not(mgt_rxready_s(i)),
        
                RX_WORDCLK_I                              => mgt_rxwordclk_s(i),
                FRAMECLK_I                                => FRAMECLK_40MHZ,         
                RX_FRAMECLK_O                             => gbt_rxframeclk_s(i), 
                RX_CLKEn_o                                => gbt_rxclkenLogic_s(i),
                     
                SYNC_I                                    => mgt_headerflag_s(i),
                CLK_ALIGN_CONFIG                          => GBTBANK_RXFRAMECLK_ALIGNPATTER_I,
                DEBUG_CLK_ALIGNMENT                       => open,
                
                PLL_LOCKED_O                              => open,
                DONE_O                                    => RX_FRAMECLK_RDY_O(i)
            );                      
        
          RX_FRAMECLK_O(i)    <= gbt_rxframeclk_s(i);
          TX_FRAMECLK_O(i)    <= gbt_txframeclk_s(i);
          
          TX_WORDCLK_O(i)     <= mgt_txwordclk_s(i);
          RX_WORDCLK_O(i)     <= mgt_rxwordclk_s(i);
                    
          gbt_rxclken_s(i)    <= mgt_headerflag_s(i) when CLOCKING_SCHEME = FULL_MGTFREQ else
                                 '1';
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
                 GBT_CLK_I                              => FRAMECLK_40MHZ,
                 TX_FRAMECLK_I                          => gbt_txframeclk_s(i),
                 TX_CLKEN_I                             => gbt_txclken_s(i),
                 RX_FRAMECLK_I                          => gbt_rxframeclk_s(i),
                 RX_CLKEN_I                             => gbt_rxclkenLogic_s(i),
                 MGTCLK_I                               => GBTBANK_MGT_DRP_CLK,
                 
                 --===============--  
                 -- Resets scheme --  
                 --===============--  
                 GENERAL_RESET_I                        => GBTBANK_GENERAL_RESET_I,
                 TX_RESET_I                             => GBTBANK_MANUAL_RESET_TX_I,
                 RX_RESET_I                             => GBTBANK_MANUAL_RESET_RX_I,
                 
                 MGT_TX_RESET_O                         => mgt_txreset_s(i),
                 MGT_RX_RESET_O                         => mgt_rxreset_s(i),
                 GBT_TX_RESET_O                         => gbt_txreset_s(i),
                 GBT_RX_RESET_O                         => gbt_rxreset_s(i),
 
                 MGT_TX_RSTDONE_I                       => mgt_txready_s(i),
                 MGT_RX_RSTDONE_I                       => mgt_rxready_s(i)                                                                    
             ); 
                      
           GBTBANK_GBTRX_READY_O(i)   <= mgt_rxready_s(i) and gbt_rxready_s(i);
           
           GBTBANK_LINK_READY_O(i)    <= mgt_txready_s(i) and mgt_rxready_s(i);
           
           GBTBANK_GBTTX_READY_O(i)   <= not(gbt_txreset_s(i));
     end generate;
   
     dataGenDs_output_gen: for i in 1 to NUM_LINKS generate
         gbt_txdata_s(i)     <= GBTBANK_GBT_DATA_I(i) when (TX_ENCODING = GBT_FRAME or (TX_ENCODING = GBT_DYNAMIC and TX_ENCODING_SEL_i(i) = '1')) else GBTBANK_WB_DATA_I(i)(115 downto 32);
         wb_txdata_s(i)      <= GBTBANK_WB_DATA_I(i)(31 downto 0);
         
         TX_DATA_O(i)        <= gbt_txdata_s(i);
         WB_DATA_O(i)        <= gbt_txdata_s(i) & wb_txdata_s(i);
     end generate;        
   
     GBTBANK_GBTRXREADY_LOST_FLAG_O                        <= (others => '0');
     GBTBANK_RXDATA_ERRORSEEN_FLAG_O                        <= (others => '0');
     GBTBANK_RXEXTRADATA_WIDEBUS_ERRORSEEN_FLAG_O        <= (others => '0');
   
     gbtBank_rxdatamap_gen: for i in 1 to NUM_LINKS generate
         GBTBANK_GBT_DATA_O(i)  <= gbt_rxdata_s(i) when RX_ENCODING = GBT_FRAME else 
                                   gbt_rxdata_s(i) when (RX_ENCODING = GBT_DYNAMIC and RX_ENCODING_SEL_i(i) = '1') else
                                   (others => '0');
        GBTBANK_WB_DATA_O(i)   <= gbt_rxdata_s(i) & wb_rxdata_s(i) when RX_ENCODING = WIDE_BUS else
                                  gbt_rxdata_s(i) & wb_rxdata_s(i) when (RX_ENCODING = GBT_DYNAMIC and RX_ENCODING_SEL_i(i) = '0') else 
                                     (others => '0');
     end generate;
            
   --=============--
   -- Transceiver --
   --=============--   
   gbtBank_mgt_gen: for i in 1 to NUM_LINKS generate
       
       mgt_devspecific_to_s.drp_addr(i)           <= "000000000";
       mgt_devspecific_to_s.drp_en(i)             <= '0';
       mgt_devspecific_to_s.drp_di(i)             <= x"0000";
       mgt_devspecific_to_s.drp_we(i)             <= '0';
       mgt_devspecific_to_s.drp_clk(i)            <= GBTBANK_MGT_DRP_CLK;
      
       mgt_devspecific_to_s.prbs_txSel(i)         <= "000";
       mgt_devspecific_to_s.prbs_rxSel(i)         <= "000";
       mgt_devspecific_to_s.prbs_txForceErr(i)    <= '0';
       mgt_devspecific_to_s.prbs_rxCntReset(i)    <= '0';
       
       mgt_devspecific_to_s.conf_diffCtrl(i)      <= "1000";    -- Comment: 807 mVppd
       mgt_devspecific_to_s.conf_postCursor(i)    <= "00000";   -- Comment: 0.00 dB (default)
       mgt_devspecific_to_s.conf_preCursor(i)     <= "00000";   -- Comment: 0.00 dB (default)
       mgt_devspecific_to_s.conf_txPol(i)         <= GBTBANK_TX_POL(i);       -- Comment: Not inverted
       mgt_devspecific_to_s.conf_rxPol(i)         <= GBTBANK_RX_POL(i);       -- Comment: Not inverted     
              
       mgt_devspecific_to_s.loopBack(i)           <= GBTBANK_LOOPBACK_I;
         
       mgt_devspecific_to_s.rx_p(i)               <= GBTBANK_MGT_RX_P(i);   
       mgt_devspecific_to_s.rx_n(i)               <= GBTBANK_MGT_RX_N(i);
       
       mgt_devspecific_to_s.reset_freeRunningClock(i)  <= GBTBANK_MGT_DRP_CLK;
                                                                
       GBTBANK_MGT_TX_P(i)                        <= mgt_devspecific_from_s.tx_p(i);  
       GBTBANK_MGT_TX_N(i)                        <= mgt_devspecific_from_s.tx_n(i);
       
       resetOnBitslip_s(i)                        <= '1' when RX_OPTIMIZATION = LATENCY_OPTIMIZED else '0';
   end generate; 
           
   --============--
   -- GBT Bank   --
   --============--
   gbt_inst: entity work.gbt_bank
     generic map(   
       NUM_LINKS                 => NUM_LINKS,
       TX_OPTIMIZATION           => TX_OPTIMIZATION,
       RX_OPTIMIZATION           => RX_OPTIMIZATION,
       TX_ENCODING               => TX_ENCODING,
       RX_ENCODING               => RX_ENCODING
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
       MGT_CLK_i                => XCVRCLK,
       GBT_TXFRAMECLK_i         => gbt_txframeclk_s,
       GBT_TXCLKEn_i            => gbt_txclken_s,
       GBT_RXFRAMECLK_i         => gbt_rxframeclk_s,
       GBT_RXCLKEn_i            => gbt_rxclken_s,
       MGT_TXWORDCLK_o          => mgt_txwordclk_s,
       MGT_RXWORDCLK_o          => mgt_rxwordclk_s,
       
       --================--
       -- GBT TX Control --
       --================--
       GBT_ISDATAFLAG_i         => GBTBANK_TX_ISDATA_SEL_I,
       TX_ENCODING_SEL_i        => TX_ENCODING_SEL_i,
       
       --=================--
       -- GBT TX Status   --
       --=================--
       TX_PHALIGNED_o          => GBTBANK_TX_ALIGNED_O,
       TX_PHCOMPUTED_o         => GBTBANK_TX_ALIGNCOMPUTED_O,
                 
       --================--
       -- GBT RX Control --
       --================--
       RX_ENCODING_SEL_i        => RX_ENCODING_SEL_i,
       
       --=================--
       -- GBT RX Status   --
       --=================--
       GBT_RXREADY_o            => gbt_rxready_s,
       GBT_ISDATAFLAG_o         => GBTBANK_RX_ISDATA_SEL_O,
       GBT_ERRORDETECTED_o      => GBTBANK_RX_ERRORDETECTED_O,
       GBT_ERRORFLAG_o          => GBTBANK_RX_BITMODIFIED_FLAG_O,
       
       --================--
       -- MGT Control    --
       --================--
       MGT_DEVSPECIFIC_i        => mgt_devspecific_to_s,
       MGT_RSTONBITSLIPEn_i     => resetOnBitslip_s,
       MGT_RSTONEVEN_i          => GBTBANK_RXBITSLIT_RSTONEVEN_I,
         
       --=================--
       -- MGT Status      --
       --=================--
       MGT_TXREADY_o            => mgt_txready_s, --GBTBANK_LINK_TX_READY_O,
       MGT_RXREADY_o            => mgt_rxready_s, --GBTBANK_LINK_RX_READY_O,
       MGT_DEVSPECIFIC_o        => mgt_devspecific_from_s,
       MGT_HEADERFLAG_o         => mgt_headerflag_s,
       MGT_RSTCNT_o             => GBTBANK_RXBITSLIP_RST_CNT_O,
       --MGT_HEADERLOCKED_o       => open,
       
       --========--
       -- Data   --
       --========--
       GBT_TXDATA_i             => gbt_txdata_s,
       GBT_RXDATA_o             => gbt_rxdata_s,
       
       WB_TXDATA_i              => wb_txdata_s,
       WB_RXDATA_o              => wb_rxdata_s
     
   );
      
end structural;
--=================================================================================================--
--#################################################################################################--
--=================================================================================================--
