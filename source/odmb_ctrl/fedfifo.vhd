-- FEDFIFO: Takes the DDU packets, and Produces continuous packets suitable for ethernet

library ieee;
library work;
library unisim;
library unimacro;
library hdlmacro;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ucsb_types.all;
use unisim.vcomponents.all;
use unimacro.vcomponents.all;
--use hdlmacro.hdlmacro.all;

entity fedfifo is
  generic (
    NFIFO : integer range 1 to 16 := 16);  -- Number of FIFOs in FEDFIFO
  port(

    clk_in  : in std_logic; --! user clock from DDU mgt (80 MHz)
    clk_out : in std_logic; --! tx user clock from B04 mgt (312.5 MHz)
    rst     : in std_logic; --! reset

    dv_in   : in std_logic;                     --! data valid from CONTROL_FSM
    ld_in   : in std_logic;                     --! indicates end of DDU packet
    data_in : in std_logic_vector(15 downto 0); --! data from CONTROL_FSM

    dv_out   : out std_logic;                    --! output data valid
    data_out : out std_logic_vector(15 downto 0) --! data output to PC(spy) mgt
    );

end fedfifo;


architecture fedfifo_architecture of fedfifo is

  component FED_PACKET_FIFO is
    port (
      SRST        : in  std_logic;
      WR_CLK      : in  std_logic;
      RD_CLK      : in  std_logic;
      DIN         : in  std_logic_vector(17 downto 0);
      WR_EN       : in  std_logic;
      RD_EN       : in  std_logic;
      DOUT        : out std_logic_vector(17 downto 0);
      FULL        : out std_logic;
      EMPTY       : out std_logic;
      WR_RST_BUSY : out std_logic;
      RD_RST_BUSY : out std_logic
      );
  end component;

  type fsm_state_type is (IDLE, FIFO_TX, DONE);
  signal fedfifo_current_state : fsm_state_type := IDLE;
  signal fedfifo_next_state    : fsm_state_type := IDLE;

  signal fedfifo_rden  : std_logic;
  signal fedfifo_out   : std_logic_vector(15 downto 0);
  signal fedfifo_ld    : std_logic;

  signal ld_in_q, ld_in_pulse : std_logic                    := '0';
  signal ld_out, ld_out_pulse : std_logic                    := '0';

  signal fifo_in, fifo_out : std_logic_vector(17 downto 0);
  signal fifo_empty        : std_logic;
  signal fifo_full         : std_logic;
  signal fifo_wren, fifo_rst         : std_logic;

  signal pkt_cnt_out : std_logic_vector(7 downto 0) := (others => '0');

  -- IDLE_ETH ensures the interframe gap of 96 bits between packets
  signal idle_cnt_en, idle_cnt_rst : std_logic             := '0';
  signal idle_cnt                  : integer range 0 to 31 := 0;

  signal dv_in_pulse, q_ld_in : std_logic := '0';
  signal first_in             : std_logic := '1';
  -- Clock cycles it takes to reach the end of the FIFO_CASCADE
  constant nwait_fifo         : integer   := NFIFO * 8;
  
begin


-- FIFOs
  DV_PULSE : PULSE2SAME port map(DOUT => dv_in_pulse, CLK_DOUT => clk_in, RST => rst, DIN => dv_in);
  FDLDIN   : FD port map(Q => ld_in_q, C => clk_in, D => ld_in); -- ld is last data

  --not sure of FDCP port order, but I don't think this bit matters -MO
  --FDFIRST : FDCP port map(Q => first_in, C => ld_in_q, CLR => dv_in_pulse, D => '1', PRE => rst);
  FDFIRST : process(ld_in, ld_in_q, dv_in_pulse, rst)
  begin
    if (ld_in_q='1') then
      first_in <= '0';
    elsif (rst='1') then
      first_in <= '1';
    elsif (ld_in='1' and ld_in_q='0') then
      first_in <= '1';
    end if;
  end process FDFIRST;
  -- first_in is 1 initally and becomes 0 after ld_in becomes 1
  
  fifo_wren <= dv_in;
  fifo_in   <= first_in & ld_in & data_in; -- Data that goes into fifo. 2 bit + 16 bit

  L1ARESETPULSE : NPULSE2FAST port map(DOUT => fifo_rst, CLK_DOUT => CLK_OUT, RST => '0', NPULSE => 5, DIN => RST); -- 5 clock pulse
  
  
  FED_FIFO : FED_PACKET_FIFO
    port map(
      SRST        => fifo_rst,
      WR_CLK      => clk_in,
      RD_CLK      => clk_out,
      DIN         => fifo_in,
      WR_EN       => fifo_wren,
      RD_EN       => fedfifo_rden, -- Determined below
      DOUT        => fifo_out,
      FULL        => fifo_full,
      EMPTY       => fifo_empty,
      WR_RST_BUSY => open,
      RD_RST_BUSY => open
      );

  fedfifo_out   <= fifo_out(15 downto 0);
  fedfifo_ld    <= fifo_out(16);

-- FSMs
  DS_LDIN     : DELAY_SIGNAL generic map (NCYCLES_MAX => nwait_fifo) port map (DOUT => q_ld_in, CLK => CLK_IN, NCYCLES => nwait_fifo, DIN => ld_in); -- DS is delayed signal
  LDIN_PULSE  : PULSE2FAST port map(DOUT => ld_in_pulse, CLK_DOUT => CLK_OUT, RST => RST, DIN => q_ld_in);
  LDOUT_PULSE : PULSE2SAME port map(DOUT => ld_out_pulse, CLK_DOUT => CLK_OUT, RST => RST, DIN => ld_out); -- ld_out is defined below.

  -- Counts number of clock cycles for a packet of data
  pkt_cnt : process (ld_in_pulse, ld_out_pulse, rst, clk_out)
    variable pkt_cnt_data : std_logic_vector(7 downto 0) := (others => '0');
  begin
    if (rst = '1') then
      pkt_cnt_data := (others => '0');
    elsif (rising_edge(clk_out)) then
      if (ld_in_pulse = '1') and (ld_out_pulse = '0') then
        pkt_cnt_data := pkt_cnt_data + 1;
      elsif (ld_in_pulse = '0') and (ld_out_pulse = '1') then
        pkt_cnt_data := pkt_cnt_data - 1;
      end if;
    end if;

    pkt_cnt_out <= pkt_cnt_data;
    
  end process;

  -- For FSM
  fedfifo_fsm_regs : process (fedfifo_next_state, rst, clk_out, idle_cnt)
  begin
    if (rst = '1') then
      fedfifo_current_state <= IDLE;
    elsif rising_edge(clk_out) then
      fedfifo_current_state <= fedfifo_next_state;
      if(idle_cnt_rst = '1') then
        idle_cnt <= 0;
      elsif(idle_cnt_en = '1') then
        idle_cnt <= idle_cnt + 1;
      end if;
    end if;
    
  end process;

  -- For FSM
  fedfifo_fsm_logic : process (fedfifo_current_state, fedfifo_out, fedfifo_ld, pkt_cnt_out, idle_cnt)
  begin
    case fedfifo_current_state is
      when IDLE =>
        dv_out       <= '0';
        data_out     <= (others => '0');
        ld_out       <= '0';
        idle_cnt_rst <= '0';
        idle_cnt_en  <= '0';
        if (pkt_cnt_out = "00000000") then
          fedfifo_rden       <= '0';
          fedfifo_next_state <= IDLE;
        else
          fedfifo_rden       <= '1';
          fedfifo_next_state <= FIFO_TX;
          data_out     <= fedfifo_out;
          dv_out       <= '1';
        end if;
        
      when FIFO_TX =>
        dv_out       <= '1';
        data_out     <= fedfifo_out;
        idle_cnt_rst <= '0';
        idle_cnt_en  <= '0';
        ld_out       <= '0';
        if (fedfifo_ld = '1') then
          fedfifo_next_state <= DONE;
          --fedfifo_rden       <= '0';
          ld_out             <= '1';
        else
          fedfifo_next_state <= FIFO_TX;
          fedfifo_rden       <= '1';
        end if;

      when DONE =>
        dv_out       <= '0';
        data_out     <= (others => '0');
        ld_out       <= '0';
        fedfifo_rden  <= '0';
        idle_cnt_en  <= '1';
        if (idle_cnt > 12) then
          fedfifo_next_state <= IDLE;
          idle_cnt_rst      <= '1';
        else
          fedfifo_next_state <= DONE;
          idle_cnt_rst      <= '0';
        end if;

      when others =>
        dv_out            <= '0';
        data_out          <= (others => '0');
        fedfifo_rden       <= '0';
        ld_out            <= '0';
        idle_cnt_rst      <= '0';
        idle_cnt_en       <= '0';
        fedfifo_next_state <= IDLE;
        
    end case;
    
  end process;

end fedfifo_architecture;
