
library ieee;
library work;
library UNISIM;
use UNISIM.vcomponents.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use work.Latches_Flipflops.all;
use work.ucsb_types.all;

--! @brief module that generates EXTPLS, INJPLS, and L1A_MATCHes for calibration purposes
--! @details Currently only used for generating INJPLS and EXTPLS signals to DCFEBs
entity CALIBTRG is
  port (
    CMSCLK      : in  std_logic;                    --! 40.079 MHz CMS clock
    CLK80       : in  std_logic;                    --! SPY clock for INJ/EXTPLS (80 MHz for 8b/10b
    RST         : in  std_logic;                    --! Reset signal
    PLSINJEN    : in  std_logic;                    --! Signal that oscillates at 1/2CMSCLK
    CCBPLS      : in  std_logic;                    --! Currently tied to 0
    CCBINJ      : in  std_logic;                    --! Currently tied to 0
    FPLS        : in  std_logic;                    --! VME signal to generate EXTPLS
    FINJ        : in  std_logic;                    --! VME signal to generate INJPLS
    FPED        : in  std_logic;                    --! VME signal to set pedestal (enables INJPLS)
    PRELCT      : in  std_logic;                    --! Currently tied to 0
    PREGTRG     : in  std_logic;                    --! Currently tied to 0
    INJ_DLY     : in  std_logic_vector(4 downto 0); --! From register in VMECONFREGS
    EXT_DLY     : in  std_logic_vector(4 downto 0); --! From register in VMECONFREGS
    CALLCT_DLY  : in  std_logic_vector(3 downto 0); --! From register in VMECONFREGS
    LCT_L1A_DLY : in  std_logic_vector(5 downto 0); --! From regsiter in VMECONFREGS
    RNDMPLS     : in  std_logic;                    --! Currently tied to 0
    RNDMGTRG    : in  std_logic;                    --! Currently tied to 0
    PEDESTAL    : out std_logic;                    --! Directly output from FPED, unused
    CAL_GTRG    : out std_logic;                    --! Calib. L1A sent to TRGCNTRL, from CCBPLS/INJ
    CALLCT      : out std_logic;                    --! Calib. LCT sent to TRGCNTRL, from CCBPLS/INJ
    INJBACK     : out std_logic;                    --! INJPLS signal sent to DCFEBs
    PLSBACK     : out std_logic;                    --! EXTPLS signal sent to DCFEBs
    LCTRQST     : out std_logic;                    --! Generated by CCBPLS/CCBINJ, not connected
    INJPLS      : out std_logic                     --! Identical to LCTRQST, not connected
    );
end CALIBTRG;

architecture CALIBTRG_arch of CALIBTRG is

  signal bc0_cmd, bc0_rst, bc0_inner                     : std_logic;
  signal start_trg_cmd, start_trg_rst, start_trg_inner   : std_logic;
  signal stop_trg_cmd, stop_trg_rst, stop_trg_inner      : std_logic;
  signal l1asrst_cmd, l1asrst_rst, l1asrst_clk_cmd       : std_logic;
  signal l1asrst_cnt_rst, l1asrst_cnt_ceo, l1asrst_inner : std_logic;
  signal l1asrst_cnt                                     : std_logic_vector(15 downto 0);
  signal ttccal_cmd, ttccal_rst, ttccal_inner            : std_logic_vector(2 downto 0);
  signal ccbinjin_1, ccbinjin_2, ccbinjin_3              : std_logic;
  signal ccbplsin_1, ccbplsin_2, ccbplsin_3              : std_logic;
  signal plsinjen_1, plsinjen_rst, plsinjen_inv          : std_logic;
  signal bx0_1                                           : std_logic;
  signal bxrst_1                                         : std_logic;
  signal clken_1                                         : std_logic;
  signal l1arst_1                                        : std_logic;

  signal logich  : std_logic                    := '1';
  signal logich4 : std_logic_vector(3 downto 0) := "1111";
  signal logic5  : std_logic_vector(3 downto 0) := "0101";

  signal finj_inv, preinj_1, preinj                                       : std_logic;
  signal fpls_inv, prepls_1, prepls                                       : std_logic;
  signal inj_cmd, inj_1, inj_2, inj_3, inj_4, inj_5_a, inj_5_b, inj_inner : std_logic;
  signal inj_cnt                                                          : std_logic_vector(15 downto 0);
  signal pls_cmd, pls_1, pls_2, pls_3, pls_4, pls_5_a, pls_5_b, pls_inner : std_logic;
  signal pls_cnt                                                          : std_logic_vector(15 downto 0);
  signal pedestal_inv                                                     : std_logic;
  signal rstpls_cmd, rstpls, rstpls_cnt_ceo, rstpls_cnt_tc, rstpls_1      : std_logic;
  signal rstpls_cnt                                                       : std_logic_vector(7 downto 0);
  signal injpls_cmd, injpls_rst, injpls_1, injpls_inner                   : std_logic;
  signal cal_gtrg_cmd, cal_gtrg_1                                         : std_logic;
  signal sr14_cnt, sr15_cnt                                               : std_logic_vector(15 downto 0);
  signal m4_out, m4_out_clk, m2_out, m2_out_clk                           : std_logic;
  signal lctrqst_inner, callct_1, callct_2, callct_3, callct_inner        : std_logic;
  signal pedestal_inner                                                   : std_logic;
  signal xl1adly_inner                                                    : std_logic_vector(1 downto 0);
begin

  --generates preinj pulse when FINJ (INJPLS) received from ODMB_VME
  -- generate preinj (finj=instr3)
  finj_inv <= not finj;
  FDCE_finj : FDCE port map(D => finj, C => cmsclk, CE => plsinjen, CLR => finj_inv, Q => preinj_1);
  FDC_preinj1 : FDC port map(D => preinj_1, C => cmsclk, CLR => rst, Q => preinj);

  --generates prepls pulse when FPLS (EXTPLS) received from ODMB_VME
  fpls_inv <= not fpls;
  FDCE_fpls : FDCE port map(D => fpls, C => cmsclk, CE => plsinjen, CLR => fpls_inv, Q => prepls_1);
  FDC_prepls_1 : FDC port map(D => prepls_1, C => cmsclk, CLR => rst, Q => prepls);

  -- generate INJPLS (INJBACK) from preinj or ccbinj pulse
  -- requires pedestal to be 0 and involves some shenanigans to get the timing from INJ_DLY at
  -- 160 MHz precision
  inj_cmd   <= preinj or ccbinj;
  FDC_logich_frominj : FDC port map(D => logich, C => inj_cmd, CLR => rstpls, Q => inj_1);
  FDC_inj_1 : FDC port map(D => inj_1, C => cmsclk, CLR => pedestal_inner, Q => inj_2);
  SRL16(inj_2, clk80, inj_dly(4 downto 1), inj_cnt, inj_cnt, inj_3);
  FD_inj_3 : FD port map(D => inj_3, C => clk80, Q => inj_4);
  FD_inj_4 : FD port map(D => inj_4, C => clk80, Q => inj_5_a);
  FD_1_inj_4 : FD_1 port map(D => inj_4, C => clk80, Q => inj_5_b);
  inj_inner <= inj_5_a when inj_dly(0) = '1' else 'Z';  -- BUFT
  inj_inner <= inj_5_b when inj_dly(0) = '0' else 'Z';  -- BUFE
  INJBACK   <= inj_inner;

  -- generates EXTPLS (PLSBACK) from prepls or ccbpls or prelct (see above)
  pls_cmd   <= prepls or ccbpls or prelct;
  FDC_logich_frompls : FDC port map(D => logich, C => pls_cmd, CLR => rstpls, Q => pls_1);
  FDC_pls_1 : FDC port map(D => pls_1, C => cmsclk, CLR => pedestal_inner, Q => pls_2);
  SRL16(pls_2, clk80, ext_dly(4 downto 1), pls_cnt, pls_cnt, pls_3);
  FD_pls_3 : FD port map(D => pls_3, C => clk80, Q => pls_4);
  FD_pls_4 : FD port map(D => pls_4, C => clk80, Q => pls_5_a);
  FD_1_pls_4 : FD_1 port map(D => pls_4, C => clk80, Q => pls_5_b);
  pls_inner <= pls_5_a when ext_dly(0) = '1' else 'Z';  -- BUFT
  pls_inner <= pls_5_b when ext_dly(0) = '0' else 'Z';  -- BUFE
  PLSBACK   <= pls_inner;

  -- pedestal enables INJPLS and EXTPLS
  -- generate pedestal (fped=instr5)
  --  pedestal_inv <= not pedestal_inner;
  --  fdc(pedestal_inv, fped, rst, pedestal_inner);
  pedestal_inner <= fped;
  PEDESTAL       <= pedestal_inner;

  -- generate rstpls. Used to clear INJPLS/EXTPLS after a certain time has elapsed (controlled by SR)
  rstpls_cmd <= pls_inner or inj_inner;
  CB8CE(cmsclk, rstpls_cmd, rstpls, rstpls_cnt, rstpls_cnt, rstpls_cnt_ceo, rstpls_cnt_tc);
  FD_rstplscnt : FD port map(D => rstpls_cnt(6), C => cmsclk, Q => rstpls_1);
  rstpls     <= rstpls_1 or rst;

  -- generate INJPLS. Confusingly, this is not the INJPLS signal that goes to the DCFEBS (INJBACK)
  -- 1 CMSCLK cycle long, triggered by ccbinj or ccbpls (currently disconnected)
  injpls_cmd <= ccbinj or ccbpls;
  FDC_logich_frominjpls : FDC port map(D => logich, C => injpls_cmd, CLR => injpls_rst, Q => injpls_1);
  FDR_injpls_1 : FDR port map(D => injpls_1, C => cmsclk, R => injpls_rst, Q => injpls_inner);
  injpls_rst <= injpls_inner or rst;
  INJPLS     <= injpls_inner;

  -- generate callct (calibration LCT) from INJPLS (above, not the DCFEB one)
  -- prelct from caltrigcon - still to be implemented
  --  lctrqst_inner <= injpls_inner or prelct;
  lctrqst_inner <= injpls_inner;
  lctrqst       <= lctrqst_inner;
  SRL16(lctrqst_inner, cmsclk, logich4, sr14_cnt, sr14_cnt, callct_1);
  FD_callct_1 : FD port map(D => callct_1, C => cmsclk, Q => callct_2);
  SRL16(callct_2, cmsclk, callct_dly, sr15_cnt, sr15_cnt, callct_3);
  FD_callct_3 : FD port map(D => callct_3, C => cmsclk, Q => callct_inner);
  CALLCT        <= callct_inner;

  -- Generate CAL_GTRG (calibration L1A) a delay LCT_L1A_DLY after calibration LCT
  LCTDLY_GTRG : LCTDLY port map(DOUT => CAL_GTRG, CLK => cmsclk, DELAY => lct_l1a_dly, DIN => callct_inner);

end CALIBTRG_arch;
