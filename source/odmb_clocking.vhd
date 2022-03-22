library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;

library unisim;
use unisim.vcomponents.all;

--! @brief ODMB7 clock management module
entity odmb_clocking is
  port (
    --------------------
    -- Input ports
    --------------------
    CMS_CLK_FPGA_P : in std_logic;      --! system clock: 40.07897 MHz
    CMS_CLK_FPGA_N : in std_logic;      --! system clock: 40.07897 MHz
    GP_CLK_6_P : in std_logic;          --! clock synthesizer ODIV6: 80 MHz
    GP_CLK_6_N : in std_logic;          --! clock synthesizer ODIV6: 80 MHz
    GP_CLK_7_P : in std_logic;          --! clock synthesizer ODIV7: 80 MHz
    GP_CLK_7_N : in std_logic;          --! clock synthesizer ODIV7: 80 MHz
    REF_CLK_1_P : in std_logic;         --! refclk0 to MGT quad 224
    REF_CLK_1_N : in std_logic;         --! refclk0 to MGT quad 224
    REF_CLK_2_P : in std_logic;         --! refclk0 to MGT quad 227
    REF_CLK_2_N : in std_logic;         --! refclk0 to MGT quad 227
    REF_CLK_3_P : in std_logic;         --! refclk0 to MGT quad 226
    REF_CLK_3_N : in std_logic;         --! refclk0 to MGT quad 226
    REF_CLK_4_P : in std_logic;         --! refclk0 to MGT quad 225
    REF_CLK_4_N : in std_logic;         --! refclk0 to MGT quad 225
    REF_CLK_5_P : in std_logic;         --! refclk1 to MGT quad 227
    REF_CLK_5_N : in std_logic;         --! refclk1 to MGT quad 227
    CLK_125_REF_P : in std_logic;       --! refclk1 to MGT quad 226
    CLK_125_REF_N : in std_logic;       --! refclk1 to MGT quad 226
    LF_CLK : in std_logic;              --! Low frequency, 10 kHz

    --------------------
    -- Output clocks
    --------------------
    mgtrefclk0_224 : out std_logic;     --! MGT refclk for R12 links from DCFEBs, may be 160.316 MHz
    mgtrefclk0_225 : out std_logic;     --! MGT refclk for R12 link  from ALCT  , may be 120.237 MHz
    mgtrefclk0_226 : out std_logic;     --! MGT refclk for SPY link  to   DDU   , may be 160.000 MHz
    mgtrefclk1_226 : out std_logic;     --! MGT refclk for SPY link  to   PC    , may be 125.000 MHz
    mgtrefclk0_227 : out std_logic;     --! MGT refclk for B04 links to   FED   , may be 160.316 MHz
    mgtrefclk1_227 : out std_logic;     --! MGT refclk for B04 links from FED   , may be 120.237 MHz      

    clk_sysclk625k : out std_logic;     --! buffered 625 kHz clock
    clk_sysclk1p25 : out std_logic;     --! buffered 1.25 MHz clock
    clk_sysclk2p5 : out std_logic;      --! buffered 2.5 MHz clock
    clk_sysclk10 : out std_logic;       --! buffered 10 MHz clock
    clk_sysclk20 : out std_logic;       --! buffered 20 MHz clock
    clk_sysclk40 : out std_logic;       --! buffered 40 MHz clock
    clk_sysclk80 : out std_logic;       --! buffered 80 MHz clock
    clk_cmsclk : out std_logic;         --! buffered CMS (40.07897 MHz) clock
    clk_lfclk : out std_logic;          --! buffered 10 kHz clock
    clk_gp6 : out std_logic;            --! buffered 80 MHz cock
    clk_gp7 : out std_logic;            --! buffered 80 MHz clock
 
    clk_mgtclk1 : out std_logic;        --! buffered clock from MGT quad 224 clock 0, from REF_CLK_1
    clk_mgtclk2 : out std_logic;        --! buffered clock from MGT quad 227 clock 0, from REF_CLK_2
    clk_mgtclk3 : out std_logic;        --! buffered clock from MGT quad 226 clock 0, from REF_CLK_3
    clk_mgtclk4 : out std_logic;        --! buffered clock from MGT quad 225 clock 0, from REF_CLK_4
    clk_mgtclk5 : out std_logic;        --! buffered clock from MGT quad 227 clock 1, from REF_CLK_5
    clk_mgtclk125 : out std_logic;      --! buffered clock from MGT quad 226 clock 1, from CLK_125_REF

    clkwiz_locked : out std_logic;      --! locked signal from the clocking wizard

    led_clkfreqs : out std_logic_vector(7 downto 0) --! blinking signals at ~1Hz for the input clocks

    );
end odmb_clocking;

architecture Clocking_Arch of odmb_clocking is

  component clockManager is
    port (
      clk_in1   : in std_logic;
      clk_out5  : out std_logic;
      clk_out10 : out std_logic;
      clk_out20 : out std_logic;
      clk_out40 : out std_logic;
      clk_out80 : out std_logic;
      locked    : out std_logic
      );
  end component;

  signal mgtrefclk0_224_odiv2 : std_logic;
  signal mgtrefclk0_225_odiv2 : std_logic;
  signal mgtrefclk0_226_odiv2 : std_logic;
  signal mgtrefclk1_226_odiv2 : std_logic;
  signal mgtrefclk0_227_odiv2 : std_logic;
  signal mgtrefclk1_227_odiv2 : std_logic;

  signal mgtclk1   : std_logic;
  signal mgtclk2   : std_logic;
  signal mgtclk3   : std_logic;
  signal mgtclk4   : std_logic;
  signal mgtclk5   : std_logic;
  signal mgtclk125 : std_logic;

  signal clk_sysclk5 : std_logic;
  signal clk2p5_unbuf : std_logic := '0';
  signal clk1p25_unbuf : std_logic := '0';
  signal clk625k_unbuf : std_logic := '0';
  signal clk2p5_inv : std_logic := '1';
  signal clk1p25_inv : std_logic := '1';
  signal clk625k_inv : std_logic := '1';

  signal clk_cmsclk_unbuf : std_logic;
  signal clk_gp6_unbuf : std_logic;
  signal clk_gp7_unbuf : std_logic;

  signal cntr_cmsclk    : unsigned(40 downto 0) := (others => '0');
  signal cntr_mgtclk1   : unsigned(40 downto 0) := (others => '0');
  signal cntr_mgtclk2   : unsigned(40 downto 0) := (others => '0');
  signal cntr_mgtclk3   : unsigned(40 downto 0) := (others => '0');
  signal cntr_mgtclk4   : unsigned(40 downto 0) := (others => '0');
  signal cntr_mgtclk5   : unsigned(40 downto 0) := (others => '0');
  signal cntr_mgtclk125 : unsigned(40 downto 0) := (others => '0');
  signal cntr_clkgp6    : unsigned(40 downto 0) := (others => '0');
  signal cntr_clkgp7    : unsigned(40 downto 0) := (others => '0');

begin

  ------------------------------------------
  -- Differential input buffers for clocks
  ------------------------------------------

  -- Special IBUF for MGT reference clocks
  u_buf_gth_q0_clk0 : IBUFDS_GTE3
    port map (
      O     => mgtrefclk0_224,
      ODIV2 => mgtrefclk0_224_odiv2,
      CEB   => '0',
      I     => REF_CLK_1_P,
      IB    => REF_CLK_1_N
      );

  u_buf_gth_q1_clk0 : IBUFDS_GTE3
    port map (
      O     => mgtrefclk0_225,
      ODIV2 => mgtrefclk0_225_odiv2,
      CEB   => '0',
      I     => REF_CLK_4_P,
      IB    => REF_CLK_4_N
      );

  u_buf_gth_q2_clk0 : IBUFDS_GTE3
    port map (
      O     => mgtrefclk0_226,
      ODIV2 => mgtrefclk0_226_odiv2,
      CEB   => '0',
      I     => REF_CLK_3_P,
      IB    => REF_CLK_3_N
      );

  u_buf_gth_q2_clk1 : IBUFDS_GTE3
    port map (
      O     => mgtrefclk1_226,
      ODIV2 => mgtrefclk1_226_odiv2,
      CEB   => '0',
      I     => CLK_125_REF_P,
      IB    => CLK_125_REF_N
      );

  u_buf_gth_q3_clk0 : IBUFDS_GTE3
    port map (
      O     => mgtrefclk0_227,
      ODIV2 => mgtrefclk0_227_odiv2,
      CEB   => '0',
      I     => REF_CLK_2_P,
      IB    => REF_CLK_2_N
      );

  u_buf_gth_q3_clk1 : IBUFDS_GTE3
    port map (
      O     => mgtrefclk1_227,
      ODIV2 => mgtrefclk1_227_odiv2,
      CEB   => '0',
      I     => REF_CLK_5_P,
      IB    => REF_CLK_5_N
      );

  -- Special IBUF for other input clocks
  u_ibufgds_gp7 : IBUFGDS
    generic map (DIFF_TERM => TRUE)
    port map (
      I => GP_CLK_7_P,
      IB => GP_CLK_7_N,
      O => clk_gp7_unbuf
    );

  u_ibufgds_gp6 : IBUFGDS
    generic map (DIFF_TERM => TRUE)
    port map (
      I => GP_CLK_6_P,
      IB => GP_CLK_6_N,
      O => clk_gp6_unbuf
      );

  -- Using the clock manager output as IBERT sysclk <- option 2
  u_ibufgds_cms : IBUFGDS
    port map (
      I => CMS_CLK_FPGA_P,
      IB => CMS_CLK_FPGA_N,
      O => clk_cmsclk_unbuf
    );

  -- Adding BUFG to the clocks
  u_bufg_gp7 : BUFG port map (I => clk_gp7_unbuf, O => clk_gp7);
  u_bufg_gp6 : BUFG port map (I => clk_gp6_unbuf, O => clk_gp6);
  u_bufg_cms : BUFG port map (I => clk_cmsclk_unbuf, O => clk_cmsclk);
  u_bufg_lfc : BUFG port map (I => LF_CLK, O => clk_lfclk);

  -- BUFG for GT clocks
  u_mgtclk0_q224 : BUFG_GT
    port map(
      I       => mgtrefclk0_224_odiv2,
      O       => mgtclk1,
      CE      => '1',
      CEMASK  => '0',
      CLR     => '0',
      CLRMASK => '0',
      DIV     => "000"
      );

  u_mgtclk0_q225 : BUFG_GT
    port map(
      I       => mgtrefclk0_225_odiv2,
      O       => mgtclk4,
      CE      => '1',
      CEMASK  => '0',
      CLR     => '0',
      CLRMASK => '0',
      DIV     => "000"
      );

  u_mgtclk0_q226 : BUFG_GT
    port map(
      I       => mgtrefclk0_226_odiv2,
      O       => mgtclk3,
      CE      => '1',
      CEMASK  => '0',
      CLR     => '0',
      CLRMASK => '0',
      DIV     => "000"
      );

  u_mgtclk1_q226 : BUFG_GT
    port map(
      I       => mgtrefclk1_226_odiv2,
      O       => mgtclk125,
      CE      => '1',
      CEMASK  => '0',
      CLR     => '0',
      CLRMASK => '0',
      DIV     => "000"
      );

  u_mgtclk0_q227 : BUFG_GT
    port map(
      I       => mgtrefclk0_227_odiv2,
      O       => mgtclk2,
      CE      => '1',
      CEMASK  => '0',
      CLR     => '0',
      CLRMASK => '0',
      DIV     => "000"
      );

  u_mgtclk1_q227 : BUFG_GT
    port map(
      I       => mgtrefclk1_227_odiv2,
      O       => mgtclk5,
      CE      => '1',
      CEMASK  => '0',
      CLR     => '0',
      CLRMASK => '0',
      DIV     => "000"
      );

  clk_mgtclk1   <= mgtclk1;
  clk_mgtclk2   <= mgtclk2;
  clk_mgtclk3   <= mgtclk3;
  clk_mgtclk4   <= mgtclk4;
  clk_mgtclk5   <= mgtclk5;
  clk_mgtclk125 <= mgtclk125;

  clockManager_i : clockManager
    port map (
      clk_in1   => clk_cmsclk_unbuf, -- input  40 MHz
      clk_out5  => clk_sysclk5,      -- output  5 MHz
      clk_out10 => clk_sysclk10,     -- output 10 MHz
      clk_out20 => clk_sysclk20,     -- output 20 MHz
      clk_out40 => clk_sysclk40,     -- output 40 MHz
      clk_out80 => clk_sysclk80,     -- output 80 MHz
      locked    => clkwiz_locked
      );


  -------------------------------------------------------------------------------------------
  -- Handle clock synthesizer signals and generate clocks
  -------------------------------------------------------------------------------------------
  -- In first version of test firmware, we will want to generate everything from 40 MHz cms clock, likely with Clock Manager IP
  -- Generate lower frequency clocks by FD
  clk2p5_inv <= not clk2p5_unbuf;
  clk1p25_inv <= not clk1p25_unbuf;
  clk625k_inv <= not clk625k_unbuf;
  FD_clk2p5 : FD port map(D => clk2p5_inv, C => clk_sysclk5, Q => clk2p5_unbuf);
  FD_clk1p25 : FD port map(D => clk1p25_inv, C => clk2p5_unbuf, Q => clk1p25_unbuf);
  FD_clk625k : FD port map(D => clk625k_inv, C => clk1p25_unbuf, Q => clk625k_unbuf);
  BUFG_clk2p5 : BUFG port map(I => clk2p5_unbuf, O => clk_sysclk2p5);
  BUFG_clk1p25 : BUFG port map(I => clk1p25_unbuf, O => clk_sysclk1p25);
  BUFG_clk625k : BUFG port map(I => clk625k_unbuf, O => clk_sysclk625k);

  -------------------------------------------------------------------------------------------
  -- Generate Human readable LED signals
  -------------------------------------------------------------------------------------------
  -- 2^25 = 33'554'432, 2^26 = 67'108'864, 2^27 = 134'217'728, 2^28 = 268'435'456, 2^30 = 1'073'741'824

  led_clkfreqs(0) <= std_logic(cntr_cmsclk(24));    -- clk at  40 MHz = led at 40/33.5 ~ 1.2 Hz
  led_clkfreqs(1) <= std_logic(cntr_mgtclk1(26));   -- clk at 160 MHz = led at 160/134 ~ 1.2 Hz 
  led_clkfreqs(2) <= std_logic(cntr_mgtclk2(26));   -- clk at 160 MHz = led at 160/134 ~ 1.2 Hz 
  led_clkfreqs(3) <= std_logic(cntr_mgtclk3(26));   -- clk at 160 MHz = led at 160/134 ~ 1.2 Hz 
  led_clkfreqs(4) <= std_logic(cntr_mgtclk4(26));   -- clk at 120 MHz = led at 120/134 ~ 0.9 Hz
  led_clkfreqs(5) <= std_logic(cntr_mgtclk5(26));   -- clk at 160 MHz = led at 160/134 ~ 1.2 Hz 
  led_clkfreqs(6) <= std_logic(cntr_mgtclk125(26)); -- clk at 125 MHz = led at 125/134 ~ 0.9 Hz
  led_clkfreqs(7) <= std_logic(cntr_clkgp7(25));    -- clk at  80 MHz = led at 80/67.1 ~ 1.2 Hz

   cntr_cmsclk    <= cntr_cmsclk    + 1 when rising_edge(clk_cmsclk_unbuf);
   cntr_mgtclk1   <= cntr_mgtclk1   + 1 when rising_edge(mgtclk1);
   cntr_mgtclk2   <= cntr_mgtclk2   + 1 when rising_edge(mgtclk2);
   cntr_mgtclk3   <= cntr_mgtclk3   + 1 when rising_edge(mgtclk3);
   cntr_mgtclk4   <= cntr_mgtclk4   + 1 when rising_edge(mgtclk4);
   cntr_mgtclk5   <= cntr_mgtclk5   + 1 when rising_edge(mgtclk5);
   cntr_mgtclk125 <= cntr_mgtclk125 + 1 when rising_edge(mgtclk125);
   cntr_clkgp6    <= cntr_clkgp6    + 1 when rising_edge(clk_gp6_unbuf);
   cntr_clkgp7    <= cntr_clkgp7    + 1 when rising_edge(clk_gp7_unbuf);

end Clocking_Arch;
