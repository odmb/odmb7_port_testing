-- EOFGEN: ouputs data with an End-Of-Packet signal in the second to last word

library ieee;
library unisim;
library unimacro;
library hdlmacro;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;
use unisim.vcomponents.all;
use unimacro.vcomponents.all;
use work.ucsb_types.all;
--use hdlmacro.hdlmacro.all;

entity eofgen is
generic (
    WORDS_PER_FIFO_WIDTH      : integer range 1 to 16 := 4 --! Shift reg depth
  );
  port(
    clk : in std_logic;
    rst : in std_logic;

    dv_in   : in std_logic;
    data_in : in std_logic_vector(15 downto 0);

    dv_out   : out std_logic;
    data_out : out std_logic_vector(71 downto 0)
    );
end eofgen;

architecture eofgen_architecture of eofgen is
  constant idle                          : std_logic_vector(17 downto 0)      := (others => '0');
  signal data_reg                        : t_std16_array(WORDS_PER_FIFO_WIDTH-1 downto 0);
  signal dv_reg                          : std_logic_vector(WORDS_PER_FIFO_WIDTH-1 downto 0);
  signal eof, eof_reg, end_of_fifo       : std_logic;
  signal width_match_counter             : std_logic_vector(1 downto 0);
  type control_state is (FIRST, SECOND, THIRD, FOURTH, END_FRAME);
  signal control_current_state, control_next_state : control_state := FIRST;
  
begin

  
        
DATA_SHIFTS : for I in 1 to WORDS_PER_FIFO_WIDTH-1 generate
    process(CLK)
    begin
        if rising_edge(CLK) and (DV_IN = '1') then
                data_reg(I) <= data_reg(I-1);
        end if;
    end process;
end generate;

CLOCK_LOGIC : process(CLK, RST)
begin
    if (RST = '1') then
        data_reg(0)         <= (others => '0');
        width_match_counter <= (others => '0');
        eof_reg             <= '0';
    
    elsif rising_edge(CLK) then
        if (DV_IN = '1') then
            data_reg(0) <= DATA_IN;
            width_match_counter <= width_match_counter + 1;
        end if;
        eof_reg     <= eof;
    end if;
end process;



  eof <= not DV_IN and dv_reg(0);
  end_of_fifo <= '1' when (width_match_counter = b"11") else '0';
  DV_OUT <= '1' when (eof_reg or end_of_fifo) = '1' else '0';
  
  
    with width_match_counter select
    DATA_OUT <= eof_reg & eof_reg & data_reg(WORDS_PER_FIFO_WIDTH-2) & eof_reg & eof_reg & data_reg(WORDS_PER_FIFO_WIDTH-1)
                & idle & idle                                                                                               
                    when b"01",
                eof_reg & eof_reg & data_reg(WORDS_PER_FIFO_WIDTH-3) & eof_reg & eof_reg & data_reg(WORDS_PER_FIFO_WIDTH-2)
                &  eof_reg & eof_reg & data_reg(WORDS_PER_FIFO_WIDTH-1) & idle              
                    when b"10",
                eof_reg & eof_reg & data_reg(WORDS_PER_FIFO_WIDTH-4) & eof_reg & eof_reg & data_reg(WORDS_PER_FIFO_WIDTH-3)
                &  eof_reg & eof_reg & data_reg(WORDS_PER_FIFO_WIDTH-2) & eof_reg & eof_reg & data_reg(WORDS_PER_FIFO_WIDTH-1)   
                    when b"11",
                eof_reg & eof_reg & data_reg(WORDS_PER_FIFO_WIDTH-1) & idle & idle & idle   
                    when others;
  
  
end eofgen_architecture;
