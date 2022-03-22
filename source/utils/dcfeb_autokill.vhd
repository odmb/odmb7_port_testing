library IEEE;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_misc.all;

library unisim;
use unisim.vcomponents.all;

library work;
use work.ucsb_types.all;

entity DCFEB_AUTOKILL is
  generic (
    NCFEB               : integer range 1 to 7 := 7  -- Number of DCFEBS, 7 for ME1/1, 5
    );
  port (
    CMSCLK              : in std_logic;
    DCFEBCLK            : in std_logic;

    DCFEB_RXD_VALID     : in std_logic_vector(NCFEB downto 1);
    DCFEB_BAD_RX        : in std_logic_vector(NCFEB downto 1);
    DCFEB_RXREADY       : in std_logic;
    KILL                : in std_logic_vector(NCFEB+2 downto 1);

    AUTOKILLED_ANY      : out std_logic_vector(15 downto 0);
    AUTOKILLED_FIBER    : out std_logic_vector(15 downto 0);
    NEW_KILL_REG        : out std_logic_vector(15 downto 0);
    UPDATE_KILL_REG     : out std_logic;
    OPTICAL_RESET       : out std_logic;

    CFG_UL_PULSE        : in std_logic;
    MAX_WORDS_DCFEB     : in std_logic_vector(15 downto 0);      --! MAX words allowed in a DCFEB data packet recieved, used for auto-kill.

    RESET               : in std_logic  --! Global reset
    );
end DCFEB_AUTOKILL;

architecture DCFEB_AUTOKILL_ARCH of DCFEB_AUTOKILL is

  component ila_1 is
    port (
      clk : in std_logic;
      probe0 : in std_logic_vector(127 downto 0)
      );
  end component;

  signal ila_data : std_logic_vector(127 downto 0);

  --------------------------------------
  -- Issues DCFEB auto-kill/recover
  --------------------------------------
  signal autokilled_dcfebs_any   : std_logic_vector(16 downto 1) := (others => '0'); -- 16 bits for readout
  signal autokilled_dcfebs_fiber : std_logic_vector(16 downto 1) := (others => '0'); -- 16 bits for readout

  signal dcfeb_rxd_valid_d       : std_logic_vector(NCFEB downto 1);
  signal kill_b                  : std_logic_vector(NCFEB downto 1);

  type t_bad_dcfeb_state is (IDLE, COUNT);
  type t_bad_dcfeb_state_arr is array (integer range <>) of t_bad_dcfeb_state;
  signal bad_dcfeb_current_state  : t_bad_dcfeb_state_arr(NCFEB downto 1);
  signal bad_dcfeb_next_state     : t_bad_dcfeb_state_arr(NCFEB downto 1);
  type t_word_cntr_arr is array (integer range <>) of integer range 0 to 5000;
  signal dcfeb_word_cnt           : t_word_cntr_arr(NCFEB downto 1);
  signal dcfeb_word_cnt_en        : std_logic_vector(NCFEB downto 1);
  signal dcfeb_word_cnt_rst       : std_logic_vector(NCFEB downto 1);
  signal bad_dcfeb_longpacket     : std_logic_vector(NCFEB downto 1);
  signal bad_dcfeb_pulse160       : std_logic_vector(NCFEB downto 1);

  signal killfeb_cntr_rst         : std_logic_vector(NCFEB downto 1);
  signal recover_cntr_rst         : std_logic_vector(NCFEB downto 1);

  signal bad_dcfeb_pulse160_fiber : std_logic_vector(NCFEB downto 1);
  signal good_dcfeb_pulse160      : std_logic_vector(NCFEB downto 1);

  signal bad_dcfeb_pulse          : std_logic_vector(NCFEB downto 1) := (others => '0');
  signal bad_dcfeb_pulse_long     : std_logic_vector(NCFEB downto 1);
  signal bad_dcfeb_pulse_q        : std_logic_vector(NCFEB downto 1) := (others => '0');
  signal bad_dcfeb_pulse_qq       : std_logic_vector(NCFEB downto 1) := (others => '0');
  signal bad_dcfeb_pulse_fiber    : std_logic_vector(NCFEB downto 1);
  signal bad_dcfeb_pulse_fiber_q  : std_logic_vector(NCFEB downto 1);
  signal bad_dcfeb_pulse_fiber_qq : std_logic_vector(NCFEB downto 1);
  signal bad_dcfeb_fiber          : std_logic_vector(NCFEB downto 1);
  signal bad_dcfeb_fiber_q        : std_logic_vector(NCFEB downto 1);
  signal bad_dcfeb_fiber_qq       : std_logic_vector(NCFEB downto 1);
  signal good_dcfeb_pulse         : std_logic_vector(NCFEB downto 1) := (others => '0');
  signal good_dcfeb_fiber         : std_logic_vector(NCFEB downto 1);
  signal good_dcfeb_fiber_q       : std_logic_vector(NCFEB downto 1);
  signal opt_reset_out            : std_logic := '0';
  signal opt_reset_cd             : std_logic := '0';

  signal new_cfeb_kill            : std_logic_vector(NCFEB downto 1);

begin

  -------------------------------------------------------------------------------------------
  -- Process internal configuration update (e.g. auto-kill)
  -------------------------------------------------------------------------------------------
  GEN_RXD : for I in 1 to NCFEB generate
    FD_DCFEBDV : FDC port map(Q => dcfeb_rxd_valid_d(I), C => DCFEBCLK, CLR => RESET, D => DCFEB_RXD_VALID(I));
  end generate GEN_RXD;


  -- This FSM counts the length of each DCFEB and if it is too long, automatically kills that DCFEB
  dcfeb_longpkt_fsm_regs : process (bad_dcfeb_next_state, RESET, DCFEBCLK,
                                    dcfeb_word_cnt_en, dcfeb_word_cnt_rst)
  begin
    for dev in NCFEB downto 1 loop
      if (reset = '1') then
        dcfeb_word_cnt(dev)           <= 0;
        bad_dcfeb_current_state(dev) <= IDLE;
      elsif rising_edge(DCFEBCLK) then
        bad_dcfeb_current_state(dev) <= bad_dcfeb_next_state(dev);
        if (dcfeb_word_cnt_rst(dev) = '1') then
          dcfeb_word_cnt(dev) <= 0;
        elsif(dcfeb_word_cnt_en(dev) = '1') then
          dcfeb_word_cnt(dev) <= dcfeb_word_cnt(dev) + 1;
        end if;
      end if;
    end loop;
  end process;

  dcfeb_longpkt_fsm_logic : process (bad_dcfeb_current_state, dcfeb_word_cnt,
                                     DCFEB_RXD_VALID, dcfeb_rxd_valid_d)
  begin
    for dev in NCFEB downto 1 loop
      dcfeb_word_cnt_en(dev)      <= '0';
      dcfeb_word_cnt_rst(dev)     <= '0';
      bad_dcfeb_longpacket(dev)   <= '0';

      case bad_dcfeb_current_state(dev) is
        when IDLE =>
          dcfeb_word_cnt_rst(dev)      <= '1';
          if (DCFEB_RXD_VALID(dev) = '1' and dcfeb_rxd_valid_d(dev) = '0') then
            bad_dcfeb_next_state(dev) <= COUNT;
          else
            bad_dcfeb_next_state(dev) <= IDLE;
          end if;
        when COUNT =>
          dcfeb_word_cnt_en(dev) <= '1';
          if (DCFEB_RXD_VALID(dev) = '0') then
            bad_dcfeb_next_state(dev) <= IDLE;
          elsif(dcfeb_word_cnt(dev) >= to_integer(unsigned(MAX_WORDS_DCFEB))) then
            bad_dcfeb_next_state(dev) <= IDLE;
            bad_dcfeb_longpacket(dev) <= '1';
          else
            bad_dcfeb_next_state(dev) <= COUNT;
          end if;
      end case;
    end loop;
  end process;


  GEN_BAD_DCFEB : for dev in NCFEB downto 1 generate
  begin
    bad_dcfeb_pulse160(dev) <= (bad_dcfeb_longpacket(dev) or bad_dcfeb_pulse160_fiber(dev)) and (not KILL(dev));
    bad_dcfeb_pulse160_fiber(dev) <= (bad_dcfeb_fiber_q(dev) and not bad_dcfeb_fiber_qq(dev));
    good_dcfeb_pulse160(dev) <= '1' when good_dcfeb_fiber(dev) = '1' and good_dcfeb_fiber_q(dev) = '0' else '0';

    -- bad_dcfeb_fiber(dev) <= DCFEB_FIBER_ERROR(dev) and not autokilled_dcfebs_fiber(dev) and not kill(dev) and not cfg_ul_pulse;
    -- good_dcfeb_fiber(dev) <= not DCFEB_FIBER_ERROR(dev) and autokilled_dcfebs_fiber(dev);

    -- Generating pulse if 32 fiber errors in 1.6 us time window to auto-kill DCFEB, sync at DCFEBCLK domain
    killfeb_cntr_rst(dev) <= RESET or autokilled_dcfebs_fiber(dev) or kill(dev) or cfg_ul_pulse;
    fiberr_killfeb_gen : TIME_AVERAGE generic map (NCYCLES => 256, THRESHOLD => 32)
      port map (DOUT => bad_dcfeb_fiber(dev), CLK => DCFEBCLK, RST => killfeb_cntr_rst(dev), DIN => DCFEB_BAD_RX(dev));
    
    -- Generating pulse if no fiber errors in 1.6 us time window and no auto-killed DCFEB
    recover_cntr_rst(dev) <= RESET or (not autokilled_dcfebs_fiber(dev));
    fiberr_recover_gen : TIME_AVERAGE generic map (NCYCLES => 256, THRESHOLD => 32)
      port map (DOUT => good_dcfeb_fiber(dev), CLK => DCFEBCLK, RST => recover_cntr_rst(dev), DIN => DCFEB_BAD_RX(dev));

    FD_BADFIBER  : FDC port map(Q => bad_dcfeb_fiber_q(dev),  C => DCFEBCLK, CLR => RESET, D => bad_dcfeb_fiber(dev));
    FD_BADFIBERQ : FDC port map(Q => bad_dcfeb_fiber_qq(dev), C => DCFEBCLK, CLR => RESET, D => bad_dcfeb_fiber_q(dev));
    FD_GOODFIBER : FDC port map(Q => good_dcfeb_fiber_q(dev), C => DCFEBCLK, CLR => RESET, D => good_dcfeb_fiber(dev));

    -- Generation of bad or good pulses on CMSCLK change KILL
    PULSE_BADDCFEB  : PULSE2SLOW port map(DOUT => bad_dcfeb_pulse(dev), CLK_DOUT => CMSCLK, CLK_DIN => DCFEBCLK, RST => RESET, DIN => bad_dcfeb_pulse160(dev));
    PULSE_BADFIBER  : PULSE2SLOW port map(DOUT => bad_dcfeb_pulse_fiber(dev), CLK_DOUT => CMSCLK, CLK_DIN => DCFEBCLK, RST => RESET, DIN => bad_dcfeb_pulse160_fiber(dev));
    PULSE_GOODDCFEB : PULSE2SLOW port map(DOUT => good_dcfeb_pulse(dev), CLK_DOUT => CMSCLK, CLK_DIN => DCFEBCLK, RST => reset, DIN => good_dcfeb_pulse160(dev));

    kill_b(dev) <= not kill(dev);
    FD_BADDCFEB  : FDC port map(Q => bad_dcfeb_pulse_q(dev),  C => CMSCLK, CLR => RESET, D => bad_dcfeb_pulse(dev));
    FD_BADDCFEB1 : FDC port map(Q => bad_dcfeb_pulse_qq(dev), C => CMSCLK, CLR => RESET, D => bad_dcfeb_pulse_q(dev));
    FD_AUTOKILL  : FDC port map(Q => autokilled_dcfebs_any(dev),  C => bad_dcfeb_pulse_qq(dev), CLR => kill_b(dev), D => '1');

    FD_BADDCFEB_FIBER  : FDC port map(Q => bad_dcfeb_pulse_fiber_q(dev),  C => CMSCLK, CLR => RESET, D => bad_dcfeb_pulse_fiber(dev));
    FD_BADDCFEB1_FIBER : FDC port map(Q => bad_dcfeb_pulse_fiber_qq(dev), C => CMSCLK, CLR => RESET, D => bad_dcfeb_pulse_fiber_q(dev));
    FD_AUTOKILL_FIBER  : FDC port map(Q => autokilled_dcfebs_fiber(dev),  C => bad_dcfeb_pulse_fiber_qq(dev), CLR => kill_b(dev), D => '1');

  end generate GEN_BAD_DCFEB;

  AUTOKILLED_ANY <= autokilled_dcfebs_any;
  AUTOKILLED_FIBER <= autokilled_dcfebs_fiber;

  UPDATE_KILL_REG <= '1' when or_reduce(bad_dcfeb_pulse) = '1' or or_reduce(good_dcfeb_pulse) = '1' else '0';

  NEW_KILL_REG(15 downto NCFEB+2)    <= (others => '0');
  NEW_KILL_REG(NCFEB+1 downto NCFEB) <= kill(NCFEB+2 downto NCFEB+1);
  NEW_KILL_REG(NCFEB-1 downto 0)     <= new_cfeb_kill;
  new_cfeb_kill <= (kill(NCFEB downto 1) or bad_dcfeb_pulse) and not good_dcfeb_pulse;

  OPTICAL_RESET <= '0';

  ila_data(1+NCFEB downto 0) <= kill;
  ila_data(8+NCFEB downto 9) <= bad_dcfeb_pulse;
  ila_data(17+NCFEB downto 18) <= good_dcfeb_pulse;
  ila_data(24+NCFEB downto 25) <= new_cfeb_kill;
  ila_data(31+NCFEB downto 32) <= bad_dcfeb_pulse160;
  ila_data(38+NCFEB downto 39) <= good_dcfeb_pulse160;
  
  ila_autokill_i : ila_1
    port map (
      clk => DCFEBCLK,
      probe0 => ila_data
      );

end DCFEB_AUTOKILL_ARCH;
