-- PCFIFO: Takes the DDU packets, and Produces continuous packets suitable for ethernet

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

entity pcfifo is
  generic (
    NFIFO : integer range 1 to 16 := 16);  -- Number of FIFOs in PCFIFO
  port(

    clk_in  : in std_logic; --! user clock from DDU mgt (80 MHz)
    clk_out : in std_logic; --! user clock from PC(spy) mgt (62.5 MHz)
    rst     : in std_logic; --! reset

    tx_ack : in std_logic; --! TX acknowledge from PC(spy) mgt, indicates end of header

    dv_in   : in std_logic;                     --! data valid from CONTROL_FSM
    ld_in   : in std_logic;                     --! indicates end of DDU packet
    data_in : in std_logic_vector(15 downto 0); --! data from CONTROL_FSM

    dv_out   : out std_logic;                    --! output data valid
    data_out : out std_logic_vector(15 downto 0) --! data output to PC(spy) mgt
    );

end pcfifo;


architecture pcfifo_architecture of pcfifo is

  component PC_ETH_PACKET_FIFO is
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

  type fsm_state_type is (IDLE, FIFO_TX, FIFO_TX_HEADER, FIFO_TX_EPKT,
                          FIFO_TX_FILL_BYTE, FIFO_TX_FILL, IDLE_ETH);
  signal pcfifo_current_state : fsm_state_type := IDLE;
  signal pcfifo_next_state    : fsm_state_type := IDLE;

  signal pcfifo_rden  : std_logic;
  signal pcfifo_empty : std_logic;
  signal pcfifo_out   : std_logic_vector(15 downto 0);
  signal pcfifo_ld    : std_logic;

  signal ld_in_q, ld_in_pulse : std_logic                    := '0';
  signal ld_out, ld_out_pulse : std_logic                    := '0';
  signal tx_ack_q             : std_logic_vector(2 downto 0) := (others => '0');
  signal tx_ack_q_b           : std_logic                    := '1';

  signal fifo_in, fifo_out : std_logic_vector(17 downto 0);
  signal fifo_empty        : std_logic;
  signal fifo_full         : std_logic;
  signal fifo_wren, fifo_rst         : std_logic;

  signal pkt_cnt_out : std_logic_vector(7 downto 0) := (others => '0');

-- Guido 10/28 => split long data packets
  signal word_cnt_en, word_cnt_rst : std_logic := '0';
  signal word_cnt                  : integer range 0 to 65535;
  signal byte_cnt                  : integer range 0 to 131070;

  -- IDLE_ETH ensures the interframe gap of 96 bits between packets
  signal idle_cnt_en, idle_cnt_rst : std_logic             := '0';
  signal idle_cnt                  : integer range 0 to 31 := 0;

  signal dv_in_pulse, q_ld_in : std_logic := '0';
  signal first_in             : std_logic := '1';
  -- Clock cycles it takes to reach the end of the FIFO_CASCADE
  constant nwait_fifo         : integer   := NFIFO * 8;

  signal epkt_cnt_total : std_logic_vector(15 downto 0) := (others => '0');
  signal epkt_cnt_en    : std_logic;
  
begin


-- FIFOs
  DV_PULSE : PULSE2SAME port map(DOUT => dv_in_pulse, CLK_DOUT => clk_in, RST => rst, DIN => dv_in);
  FDLDIN   : FD port map(Q => ld_in_q, C => clk_in, D => ld_in);

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
  
  
  fifo_wren <= dv_in;
  fifo_in   <= first_in & ld_in & data_in;

  L1ARESETPULSE : NPULSE2FAST port map(DOUT => fifo_rst, CLK_DOUT => CLK_OUT, RST => '0', NPULSE => 5, DIN => RST);
  
  
  --PC_FIFO_CASCADE : FIFO_CASCADE
  --  generic map (
  --    NFIFO        => NFIFO,            -- number of FIFOs in cascade
  --    DATA_WIDTH   => 18,               -- With of data packets
  --    WR_FASTER_RD => true)   -- Set int_clk to WRCLK if faster than RDCLK

  --  port map(
  --    DO        => fifo_out,            -- Output data
  --    EMPTY     => fifo_empty,          -- Output empty
  --    FULL      => fifo_full,           -- Output full
  --    HALF_FULL => open,

  --    DI    => fifo_in,                 -- Input data
  --    RDCLK => clk_out,                 -- Input read clock
  --    RDEN  => pcfifo_rden,             -- Input read enable
  --    RST   => fifo_rst,                     -- Input reset
  --    WRCLK => clk_in,                  -- Input write clock
  --    WREN  => fifo_wren                -- Input write enable
  --    );
  
  PC_FIFO : PC_ETH_PACKET_FIFO
    port map(
      SRST        => fifo_rst,
      WR_CLK      => clk_in,
      RD_CLK      => clk_out,
      DIN         => fifo_in,
      WR_EN       => fifo_wren,
      RD_EN       => pcfifo_rden,
      DOUT        => fifo_out,
      FULL        => fifo_full,
      EMPTY       => fifo_empty,
      WR_RST_BUSY => open,
      RD_RST_BUSY => open
      );

  pcfifo_out   <= fifo_out(15 downto 0);
  pcfifo_ld    <= fifo_out(16);
  pcfifo_empty <= fifo_empty;

  FDCACK   : FDC port map(Q => tx_ack_q(0), C => tx_ack, CLR => tx_ack_q(2), D => tx_ack_q_b);
  FDACK_Q  : FD port map(Q => tx_ack_q(1), C => clk_out, D => tx_ack_q(0));
  FDACK_QQ : FD port map(Q => tx_ack_q(2), C => clk_out, D => tx_ack_q(1));
  tx_ack_q_b <= not tx_ack_q(2);

-- FSMs
  DS_LDIN     : DELAY_SIGNAL generic map (NCYCLES_MAX => nwait_fifo) port map (DOUT => q_ld_in, CLK => CLK_IN, NCYCLES => nwait_fifo, DIN => ld_in);
  LDIN_PULSE  : PULSE2SLOW port map(DOUT => ld_in_pulse, CLK_DOUT => CLK_OUT, CLK_DIN => CLK_IN, RST => RST, DIN => q_ld_in);
  LDOUT_PULSE : PULSE2SAME port map(DOUT => ld_out_pulse, CLK_DOUT => CLK_OUT, RST => RST, DIN => ld_out);

-- Generation of counter for total packets sent
  epkt_cnt_total_pro : process (rst, clk_out, epkt_cnt_en)
  begin
    if (rst = '1') then
      epkt_cnt_total <= (others => '0');
    elsif (rising_edge(clk_out)) then
      if (epkt_cnt_en = '1') then
        epkt_cnt_total <= epkt_cnt_total + 1;
      end if;
    end if;
  end process;

-- Counter to split long data packets
  word_cnt_pro : process (word_cnt_rst, word_cnt_en, rst, clk_out)
  begin
    if (rst = '1') then
      word_cnt <= 0;
    elsif (rising_edge(clk_out)) then
      if (word_cnt_rst = '1') then
        word_cnt <= 0;
      elsif (word_cnt_en = '1') then
        word_cnt <= word_cnt + 1;
      end if;
    end if;
  end process;
  byte_cnt <= 2*(word_cnt+2)-2;

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

  pcfifo_fsm_regs : process (pcfifo_next_state, rst, clk_out, idle_cnt)
  begin
    if (rst = '1') then
      pcfifo_current_state <= IDLE;
    elsif rising_edge(clk_out) then
      pcfifo_current_state <= pcfifo_next_state;
      if(idle_cnt_rst = '1') then
        idle_cnt <= 0;
      elsif(idle_cnt_en = '1') then
        idle_cnt <= idle_cnt + 1;
      end if;
    end if;
    
  end process;

  pcfifo_fsm_logic : process (pcfifo_current_state, pcfifo_out, pcfifo_empty, pcfifo_ld,
                              pkt_cnt_out, tx_ack_q, idle_cnt, word_cnt)
  begin
    case pcfifo_current_state is
      when IDLE =>
        dv_out       <= '0';
        data_out     <= (others => '0');
        ld_out       <= '0';
        idle_cnt_rst <= '0';
        idle_cnt_en  <= '0';
        word_cnt_rst <= '1';
        word_cnt_en  <= '0';
        epkt_cnt_en  <= '0';
        if (pkt_cnt_out = "00000000") then
          pcfifo_rden       <= '0';
          pcfifo_next_state <= IDLE;
        else
          pcfifo_rden       <= '1';
          pcfifo_next_state <= FIFO_TX_HEADER;
        end if;
        
      when FIFO_TX_HEADER =>
        dv_out       <= '1';
        data_out     <= pcfifo_out;
        ld_out       <= '0';
        idle_cnt_rst <= '0';
        idle_cnt_en  <= '0';
        word_cnt_rst <= '0';
        word_cnt_en  <= '0';
        epkt_cnt_en  <= '0';
        if (tx_ack_q(0) = '1') then
          pcfifo_rden       <= '1';
          pcfifo_next_state <= FIFO_TX;
        else
          pcfifo_rden       <= '0';
          pcfifo_next_state <= FIFO_TX_HEADER;
        end if;

      when FIFO_TX =>
        dv_out       <= '1';
        data_out     <= pcfifo_out;
        idle_cnt_rst <= '0';
        idle_cnt_en  <= '0';
        word_cnt_rst <= '0';
        word_cnt_en  <= '1';
        epkt_cnt_en  <= '0';
        ld_out       <= '0';
        -- The total has to be multiple of 4, but word_cnt = total-2
        -- switched max word_cnt from 4002 to 742 since new ethernet card doesn't like jumbo packets
        if (pcfifo_ld = '1' and word_cnt > 26) or word_cnt = 742 then
          pcfifo_next_state <= FIFO_TX_EPKT;
          pcfifo_rden       <= '0';
        elsif pcfifo_ld = '1' and word_cnt <= 30 then
          pcfifo_next_state <= FIFO_TX_FILL_BYTE;
          pcfifo_rden       <= '0';
        else
          pcfifo_next_state <= FIFO_TX;
          pcfifo_rden       <= '1';
        end if;

      when FIFO_TX_FILL_BYTE =>
        dv_out            <= '1';
        data_out          <= x"FF" & std_logic_vector(to_unsigned(byte_cnt, 8));
        idle_cnt_rst      <= '0';
        idle_cnt_en       <= '0';
        word_cnt_rst      <= '0';
        word_cnt_en       <= '1';
        epkt_cnt_en       <= '0';
        ld_out            <= '0';
        pcfifo_rden       <= '0';
        pcfifo_next_state <= FIFO_TX_FILL;

      when FIFO_TX_FILL =>
        dv_out       <= '1';
        data_out     <= x"FFFF";
        idle_cnt_rst <= '0';
        idle_cnt_en  <= '0';
        word_cnt_rst <= '0';
        word_cnt_en  <= '1';
        epkt_cnt_en  <= '0';
        ld_out       <= '0';
        pcfifo_rden  <= '0';
        if word_cnt = 30 then
          pcfifo_next_state <= FIFO_TX_EPKT;
        else
          pcfifo_next_state <= FIFO_TX_FILL;
        end if;

      when FIFO_TX_EPKT =>
        dv_out   <= '1';
        data_out <= epkt_cnt_total;
        if (pcfifo_ld = '0') then
          word_cnt_rst <= '0';
          ld_out       <= '0';
        else
          word_cnt_rst <= '1';
          ld_out       <= '1';
        end if;
        idle_cnt_rst      <= '0';
        idle_cnt_en       <= '0';
        word_cnt_en       <= '0';
        epkt_cnt_en       <= '1';
        pcfifo_rden       <= '0';
        pcfifo_next_state <= IDLE_ETH;

      when IDLE_ETH =>
        dv_out       <= '0';
        data_out     <= (others => '0');
        ld_out       <= '0';
        pcfifo_rden  <= '0';
        idle_cnt_en  <= '1';
        epkt_cnt_en  <= '0';
        word_cnt_rst <= '0';
        word_cnt_en  <= '0';
        if (idle_cnt > 12) then
          pcfifo_next_state <= IDLE;
          idle_cnt_rst      <= '1';
        else
          pcfifo_next_state <= IDLE_ETH;
          idle_cnt_rst      <= '0';
        end if;

      when others =>
        dv_out            <= '0';
        data_out          <= (others => '0');
        pcfifo_rden       <= '0';
        ld_out            <= '0';
        idle_cnt_rst      <= '0';
        idle_cnt_en       <= '0';
        word_cnt_rst      <= '0';
        word_cnt_en       <= '0';
        epkt_cnt_en       <= '0';
        pcfifo_next_state <= IDLE;
        
    end case;
    
  end process;

end pcfifo_architecture;
