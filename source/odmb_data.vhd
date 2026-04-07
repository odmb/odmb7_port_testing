library IEEE;
library work;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;

library unisim;
use unisim.vcomponents.all;
use work.ucsb_types.all;

entity odmb_data is
  generic (
    NCFEB               : integer range 1 to 7 := 7  -- Number of DCFEBS, 7 for ME1/1, 5
  );
  port (
    CMSCLK              : in std_logic;
    DDUCLK              : in std_logic;
    CLK_160             : in std_logic;
    DCFEBCLK            : in std_logic;
    RESET               : in std_logic;
    L1ACNT_RST          : in std_logic;
    KILL                : in std_logic_vector(NCFEB+2 downto 1);
    CAFIFO_L1A          : in std_logic;
    CAFIFO_L1A_MATCH_IN : in std_logic_vector(NCFEB+2 downto 1);
    DCFEB_L1A           : in std_logic;
    DCFEB_L1A_MATCH     : in std_logic_vector(NCFEB downto 1);
    NWORDS_DUMMY        : in std_logic_vector(15 downto 0);

    DCFEB_TCK           : in std_logic_vector(NCFEB downto 1);
    DCFEB_TDO           : out std_logic_vector(NCFEB downto 1);
    DCFEB_TMS           : in std_logic;
    DCFEB_TDI           : in std_logic;

    DCFEB_FIFO_RST      : in std_logic_vector (NCFEB downto 1); -- auto-kill related
    EOF_DATA            : out std_logic_vector(NCFEB+2 downto 1);
    INTO_FIFO_DAV       : out std_logic_vector(NCFEB+2 downto 1);

    OTMB_DATA_IN        : in std_logic_vector(17 downto 0);
    ALCT_DATA_IN        : in std_logic_vector(17 downto 0);
    DCFEB_DATA_IN       : in t_std16_array(NCFEB downto 1);
    DCFEB_DAV_IN        : in std_logic_vector(NCFEB downto 1);

    GEN_DCFEB_SEL       : in std_logic;

    FIFO_RE_B           : in std_logic_vector(NCFEB+2 downto 1);
    FIFO_OE_B           : in std_logic_vector(NCFEB+2 downto 1);
    FIFO_DOUT           : out std_logic_vector(17 downto 0);
    FIFO_EMPTY          : out std_logic_vector(NCFEB+2 downto 1);
    FIFO_HALF_FULL      : out std_logic_vector(NCFEB+2 downto 1);
    FIFO_FULL           : out std_logic_vector(NCFEB+2 downto 1)
  );
end odmb_data;

architecture ODMB_DATA_ARCH of odmb_data is

  component datafifo_40mhz
    port (
      srst : in std_logic;
      wr_clk : in std_logic;
      rd_clk : in std_logic;
      din : in std_logic_vector(17 downto 0);
      wr_en : in std_logic;
      rd_en : in std_logic;
      dout : out std_logic_vector(17 downto 0);
      full : out std_logic;
      empty : out std_logic;
      prog_full : out std_logic;
      wr_rst_busy : out std_logic;
      rd_rst_busy : out std_logic
      );
  end component;

  component datafifo_dcfeb
    port (
      srst : in std_logic;
      wr_clk : in std_logic;
      rd_clk : in std_logic;
      din : in std_logic_vector(17 downto 0);
      wr_en : in std_logic;
      rd_en : in std_logic;
      dout : out std_logic_vector(17 downto 0);
      full : out std_logic;
      empty : out std_logic;
      prog_full : out std_logic;
      wr_rst_busy : out std_logic;
      rd_rst_busy : out std_logic
      );
  end component;

  component EOFGEN is
    port(
      clk : in std_logic;
      rst : in std_logic;

      dv_in   : in std_logic;
      data_in : in std_logic_vector(15 downto 0);

      dv_out   : out std_logic;
      data_out : out std_logic_vector(17 downto 0)
      );

  end component;

  component DCFEB_V6 is
    generic (
      dcfeb_addr : std_logic_vector(3 downto 0) := "1000"  -- DCFEB address
      );
    port (
      clk           : in std_logic;
      dcfebclk      : in std_logic;
      rst           : in std_logic;
      l1a           : in std_logic;
      l1a_match     : in std_logic;
      tx_ack        : in std_logic;
      nwords_dummy  : in std_logic_vector(15 downto 0);

      dcfeb_dv      : out std_logic;
      dcfeb_data    : out std_logic_vector(15 downto 0);
      adc_mask      : out std_logic_vector(11 downto 0);
      dcfeb_fsel    : out std_logic_vector(63 downto 0);
      dcfeb_jtag_ir : out std_logic_vector(9 downto 0);
      trst          : in  std_logic;
      tck           : in  std_logic;
      tms           : in  std_logic;
      tdi           : in  std_logic;
      tdo           : out std_logic;
      rtn_shft_en   : out std_logic;
      done          : out std_logic;
      injpls        : in std_logic;
      extpls        : in std_logic;
      bc0           : in std_logic;
      resync        : in std_logic;
      diagout       : out std_logic_vector(17 downto 0)
       );
  end component;

  component alct_otmb_data_gen is
    port(
      clk            : in std_logic;
      rst            : in std_logic;
      l1a            : in std_logic;
      alct_l1a_match : in std_logic;
      otmb_l1a_match : in std_logic;
      nwords_dummy   : in std_logic_vector(15 downto 0);

      alct_dv   : out std_logic;
      alct_data : out std_logic_vector(15 downto 0);
      otmb_dv   : out std_logic;
      otmb_data : out std_logic_vector(15 downto 0));
  end component;

  --signal gen_dcfeb_sel       : std_logic := '0';

  signal rx_alct_data_valid  : std_logic;
  signal alct_data_valid     : std_logic;
  signal gen_alct_data_valid : std_logic;

  signal gen_alct_data       : std_logic_vector(15 downto 0);
  signal alct_data           : std_logic_vector(15 downto 0);
  signal alct_q              : std_logic_vector(17 downto 0);
  signal alct_qq             : std_logic_vector(17 downto 0);

  signal alct_fifo_data_valid : std_logic;
  signal alct_fifo_data_in    : std_logic_vector(17 downto 0);
  signal alct_fifo_full       : std_logic;
  signal alct_fifo_empty      : std_logic;

  signal rx_otmb_data_valid  : std_logic;
  signal otmb_data_valid     : std_logic;
  signal gen_otmb_data_valid : std_logic;

  signal otmb_data           : std_logic_vector(15 downto 0);
  signal gen_otmb_data       : std_logic_vector(15 downto 0);
  signal otmb_q              : std_logic_vector(17 downto 0);
  signal otmb_qq             : std_logic_vector(17 downto 0);

  signal otmb_fifo_data_valid : std_logic;
  signal otmb_fifo_data_in    : std_logic_vector(17 downto 0);
  signal otmb_fifo_full       : std_logic;
  signal otmb_fifo_empty      : std_logic;

  signal dcfeb_fifo_empty  : std_logic_vector(NCFEB downto 1);
  signal dcfeb_fifo_full   : std_logic_vector(NCFEB downto 1);

  type dcfeb_addr_type is array (1 to 7) of std_logic_vector(3 downto 0);
  constant dcfeb_addr  : dcfeb_addr_type := ("0001", "0010", "0011", "0100", "0101", "0110", "0111");
  constant push_dly    : integer := 63;  -- It needs to be > alct/otmb_push_dly
  constant push_dlyp4  : integer := push_dly+4;  -- push_dly+4

  signal gen_dcfeb_data : t_std16_array(NCFEB downto 1);
  signal rx_dcfeb_data  : t_std16_array(NCFEB downto 1);

  signal gen_dcfeb_data_valid                 : std_logic_vector(NCFEB downto 1);
  signal dcfeb_data_valid_d, dcfeb_data_valid : std_logic_vector(NCFEB downto 1);

  type dcfeb_adc_mask_type is array (NCFEB downto 1) of std_logic_vector(11 downto 0);
  signal dcfeb_adc_mask : dcfeb_adc_mask_type;

  type dcfeb_fsel_type is array (NCFEB downto 1) of std_logic_vector(63 downto 0);
  signal dcfeb_fsel : dcfeb_fsel_type;

  type dcfeb_jtag_ir_type is array (NCFEB downto 1) of std_logic_vector(9 downto 0);
  signal dcfeb_jtag_ir : dcfeb_jtag_ir_type;

  signal gen_tdo : std_logic_vector(NCFEB downto 1) := (others => '0');

  signal dcfeb_fifo_in : t_std16_array(NCFEB downto 1);
  signal dcfeb_data    : t_std16_array(NCFEB downto 1);

  signal eofgen_dcfeb_fifo_in    : t_std18_array(NCFEB downto 1);
  signal eofgen_dcfeb_data_valid : std_logic_vector(NCFEB downto 1);
  signal dcfeb_fifo_out          : t_std18_array(NCFEB downto 1);
  signal alct_fifo_data_out      : std_logic_vector(17 downto 0);
  signal otmb_fifo_data_out      : std_logic_vector(17 downto 0);

  signal data_fifo_re            : std_logic_vector (NCFEB+2 downto 1);
  signal data_fifo_we            : std_logic_vector (NCFEB+2 downto 1);
  signal pulse_eof40, pulse_eof160  : std_logic_vector(NCFEB downto 1);

  -- signal l1acnt_rst       : std_logic := '0';
  signal fifo_full_cmsclk : std_logic_vector(NCFEB+2 downto 1);
  signal l1acnt_fifo_rst : std_logic := '0';
  signal datafifo_mask   : std_logic := '0';

begin


  L1ARESETPULSE : RESET_FIFO
    generic map (
      NCLOCKS => 10
      )
    port map (
      FIFO_RST => l1acnt_fifo_rst,
      FIFO_MASK => datafifo_mask,
      CLK => CMSCLK,
      IN_RST => L1ACNT_RST
      );

  -- gen ALCT/OTMB data
  ALCT_OTMB_DATA_GEN_PM : alct_otmb_data_gen
    port map(
      clk            => cmsclk,
      rst            => reset,
      l1a            => cafifo_l1a,
      alct_l1a_match => cafifo_l1a_match_in(NCFEB+2),
      otmb_l1a_match => cafifo_l1a_match_in(NCFEB+1),
      nwords_dummy   => nwords_dummy,

      alct_dv        => gen_alct_data_valid,
      alct_data      => gen_alct_data,
      otmb_dv        => gen_otmb_data_valid,
      otmb_data      => gen_otmb_data
      );

  ALCT_OTMB_SYNC : process (CMSCLK)
  begin
    if rising_edge(CMSCLK) then
      alct_q <= alct_data_in;
      otmb_q <= otmb_data_in;
      alct_qq <= alct_q;
      otmb_qq <= otmb_q;
    end if;
  end process;

  rx_alct_data_valid <= not alct_qq(17);
  alct_data_valid    <= '0' when KILL(NCFEB+2) = '1' else
                        rx_alct_data_valid when (GEN_DCFEB_SEL = '0') else
                        gen_alct_data_valid;

  alct_data <= alct_qq(15 downto 0) when (GEN_DCFEB_SEL = '0') else gen_alct_data;

  rx_otmb_data_valid <= not otmb_qq(17);
  otmb_data_valid    <= '0' when KILL(NCFEB+1) = '1' else
                        rx_otmb_data_valid when (GEN_DCFEB_SEL = '0') else
                        gen_otmb_data_valid;

  otmb_data <= otmb_qq(15 downto 0) when (GEN_DCFEB_SEL = '0') else gen_otmb_data;

  ALCT_EOFGEN_PM : EOFGEN
    port map (
      clk => cmsclk,
      rst => reset,

      dv_in   => alct_data_valid,
      data_in => alct_data,

      dv_out   => alct_fifo_data_valid,
      data_out => alct_fifo_data_in
      );

  OTMB_EOFGEN_PM : EOFGEN
    port map (
      clk => cmsclk,
      rst => reset,

      dv_in   => otmb_data_valid,
      data_in => otmb_data,

      dv_out   => otmb_fifo_data_valid,
      data_out => otmb_fifo_data_in
      );

  data_fifo_we(NCFEB+2) <= alct_fifo_data_valid and datafifo_mask;
  data_fifo_we(NCFEB+1) <= otmb_fifo_data_valid and datafifo_mask;

  -- datafifo for alct and otmb
  datafifo_alct_pm : datafifo_40mhz
    port map(
      srst      => l1acnt_fifo_rst,
      wr_clk    => CMSCLK,
      rd_clk    => DDUCLK,
      din       => alct_fifo_data_in,
      wr_en     => data_fifo_we(NCFEB+2),
      rd_en     => data_fifo_re(NCFEB+2),
      dout      => alct_fifo_data_out,
      full      => FIFO_FULL_CMSCLK(NCFEB+2), --alct_fifo_full,
      empty     => FIFO_EMPTY(NCFEB+2),
      prog_full => FIFO_HALF_FULL(NCFEB+2)
      );

  datafifo_otmb_pm : datafifo_40mhz
    port map(
      srst      => l1acnt_fifo_rst,
      wr_clk    => CMSCLK,
      rd_clk    => DDUCLK,
      din       => otmb_fifo_data_in,
      wr_en     => data_fifo_we(NCFEB+1),
      rd_en     => data_fifo_re(NCFEB+1),
      dout      => otmb_fifo_data_out,
      full      => FIFO_FULL_CMSCLK(NCFEB+1), --otmb_fifo_full,
      empty     => FIFO_EMPTY(NCFEB+1),
      prog_full => FIFO_HALF_FULL(NCFEB+1)
      );

  -- eof_data
  PULSEEOFALCT : PULSE2SAME port map(DOUT => EOF_DATA(NCFEB+2), CLK_DOUT => CMSCLK, RST => RESET, DIN => alct_fifo_data_in(17));
  PULSEEOFOTMB : PULSE2SAME port map(DOUT => EOF_DATA(NCFEB+1), CLK_DOUT => CMSCLK, RST => RESET, DIN => otmb_fifo_data_in(17));

  GEN_DCFEB : for I in NCFEB downto 1 generate
  begin

    DCFEB_V6_PM : DCFEB_V6
      generic map(
        dcfeb_addr    => dcfeb_addr(I)
        )
      port map(
        clk           => cmsclk,
        dcfebclk      => dcfebclk,
        rst           => reset,
        l1a           => dcfeb_l1a,
        l1a_match     => dcfeb_l1a_match(I),
        tx_ack        => '1',
        nwords_dummy  => nwords_dummy,

        dcfeb_dv      => gen_dcfeb_data_valid(I),
        dcfeb_data    => gen_dcfeb_data(I),
        adc_mask      => dcfeb_adc_mask(I),
        dcfeb_fsel    => dcfeb_fsel(I),
        dcfeb_jtag_ir => dcfeb_jtag_ir(I),
        trst          => reset,
        tck           => dcfeb_tck(I),
        tms           => dcfeb_tms,
        tdi           => dcfeb_tdi,
        tdo           => gen_tdo(I),
        rtn_shft_en   => open,
        done          => open,
        injpls        => '0',
        extpls        => '0',
        bc0           => '0',
        resync        => '0',
        diagout       => open
        );

    dcfeb_data_valid_d(I) <= '0' when KILL(I) = '1' else
                             DCFEB_DAV_IN(I) when GEN_DCFEB_SEL = '0' else
                             gen_dcfeb_data_valid(I);

    dcfeb_data(I) <= DCFEB_DATA_IN(I) when (gen_dcfeb_sel = '0') else gen_dcfeb_data(I);

    FD_DCFEBDV   : FDC port map (Q => dcfeb_data_valid(I), C => DCFEBCLK, CLR => reset, D => dcfeb_data_valid_d(I));
    FD_DCFEBDATA : FDVEC port map (DOUT => dcfeb_fifo_in(I), CLK => DCFEBCLK, RST => reset, DIN => dcfeb_data(I));

    --masked_l1a_match(I) <= '0' when mask_l1a(I) = '1' else int_l1a_match(I);
    --DS_L1AMATCH : DELAY_SIGNAL generic map(1)
    --  port map(DCFEB_L1A_MATCH(I), cmsclk, cable_dly, masked_l1a_match(I));

    --int_tdo(I) <= dcfeb_tdo(I) when (gen_dcfeb_sel = '0') else gen_tdo(I);
    dcfeb_tdo(I) <= gen_tdo(I);

    EOFGEN_PM : EOFGEN
      port map (
        clk => DCFEBCLK,
        rst => reset,

        dv_in   => dcfeb_data_valid(I),
        data_in => dcfeb_fifo_in(I),

        dv_out   => eofgen_dcfeb_data_valid(I),
        data_out => eofgen_dcfeb_fifo_in(I)
        );

    data_fifo_we(I) <= eofgen_dcfeb_data_valid(I) and datafifo_mask and not dcfeb_fifo_rst(I);

    datafifo_dcfeb_pm : datafifo_dcfeb
      port map(
        --need change due to use auto-kill
        --rst       => dcfeb_fifo_rst(I),
        srst      => l1acnt_fifo_rst,
        wr_clk    => DCFEBCLK,
        rd_clk    => DDUCLK,
        din       => eofgen_dcfeb_fifo_in(I),
        wr_en     => data_fifo_we(I),
        rd_en     => data_fifo_re(I),
        dout      => dcfeb_fifo_out(I),
        full      => FIFO_FULL_CMSCLK(I), --dcfeb_fifo_full(I),
        empty     => FIFO_EMPTY(I),
        prog_full => FIFO_HALF_FULL(I)
        );

    --pulse_eof160(i) <= eofgen_dcfeb_fifo_in(I)(17) and not kill(i) and not bad_dcfeb_pulse_long(i);
    pulse_eof160(i) <= eofgen_dcfeb_fifo_in(I)(17) and not kill(i);
    PULSEEOFDCFEB : PULSE2SLOW port map(DOUT => pulse_eof40(i), CLK_DOUT => cmsclk, CLK_DIN => dcfebclk, RST => reset, DIN => pulse_eof160(i));
    DS_EOF_PUSH   : DELAY_SIGNAL generic map(push_dlyp4) port map(DOUT => EOF_DATA(I), CLK => cmsclk, NCYCLES => push_dlyp4, DIN => pulse_eof40(I));

  end generate GEN_DCFEB;

  INTO_FIFO_DAV(NCFEB+2)        <= alct_fifo_data_valid;
  INTO_FIFO_DAV(NCFEB+1)        <= otmb_fifo_data_valid;
  INTO_FIFO_DAV(NCFEB downto 1) <= eofgen_dcfeb_data_valid;

  GENFIFORE : for index in 1 to NCFEB+2 generate
  begin
    data_fifo_re(index) <= datafifo_mask and not fifo_re_b(index);
  end generate GENFIFORE;
  
  FIFO_FULL_CDC: for dev in NCFEB+1 to NCFEB+2 generate
    CDC_PER_FULL: CROSSCLOCK port map(DOUT => FIFO_FULL(dev), CLK_DOUT=> DDUCLK, CLK_DIN=>CMSCLK, RST=> L1ACNT_FIFO_RST, DIN=>FIFO_FULL_CMSCLK(dev));
  end generate FIFO_FULL_CDC;  
  
  FIFO_FULL_CDC_DCFEB: for dev in 1 to NCFEB generate
    CDC_PER_FULL: CROSSCLOCK port map(DOUT => FIFO_FULL(dev), CLK_DOUT=> DDUCLK, CLK_DIN=>CLK_160, RST=> L1ACNT_FIFO_RST, DIN=>FIFO_FULL_CMSCLK(dev));
  end generate FIFO_FULL_CDC_DCFEB;

  -------------------------------------------------------------------------------------------
  -- Handle data readout
  -------------------------------------------------------------------------------------------

  -- Use for loop to decode one-hot code of variable length (may not be the most efficient way)
  -- l_fifo_out : process (FIFO_OE_B)
  -- begin
  --   FIFO_DOUT <= (others => '0');
  --   for i in 1 to NCFEB loop
  --     if (FIFO_OE_B(i) = '0') then
  --       FIFO_DOUT <= dcfeb_fifo_out(I);
  --     end if;
  --   end loop;
  --   if (FIFO_OE_B(NCFEB+1) = '0') then
  --     FIFO_DOUT <= otmb_fifo_data_out;
  --   end if;
  --   if (FIFO_OE_B(NCFEB+2) = '0') then
  --     FIFO_DOUT <= alct_fifo_data_out;
  --   end if;
  -- end process;

  u_dout_assign_7 : if NCFEB = 7 generate
    FIFO_DOUT <= dcfeb_fifo_out(1)  when FIFO_OE_B = "111111110" else
                 dcfeb_fifo_out(2)  when FIFO_OE_B = "111111101" else
                 dcfeb_fifo_out(3)  when FIFO_OE_B = "111111011" else
                 dcfeb_fifo_out(4)  when FIFO_OE_B = "111110111" else
                 dcfeb_fifo_out(5)  when FIFO_OE_B = "111101111" else
                 dcfeb_fifo_out(6)  when FIFO_OE_B = "111011111" else
                 dcfeb_fifo_out(7)  when FIFO_OE_B = "110111111" else
                 otmb_fifo_data_out when FIFO_OE_B = "101111111" else
                 alct_fifo_data_out when FIFO_OE_B = "011111111" else
                 (others => '0');
  end generate;

  u_dout_assign_5 : if NCFEB = 5 generate
    FIFO_DOUT <= dcfeb_fifo_out(1)  when FIFO_OE_B = "1111110" else
                 dcfeb_fifo_out(2)  when FIFO_OE_B = "1111101" else
                 dcfeb_fifo_out(3)  when FIFO_OE_B = "1111011" else
                 dcfeb_fifo_out(4)  when FIFO_OE_B = "1110111" else
                 dcfeb_fifo_out(5)  when FIFO_OE_B = "1101111" else
                 otmb_fifo_data_out when FIFO_OE_B = "1011111" else
                 alct_fifo_data_out when FIFO_OE_B = "0111111" else
                 (others => '0');
  end generate;

  -- fifo_empty <= alct_fifo_empty & otmb_fifo_empty & dcfeb_fifo_empty;

end ODMB_DATA_ARCH;
