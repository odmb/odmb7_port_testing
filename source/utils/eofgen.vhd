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
    data_out : out std_logic_vector(63 downto 0);
    eof_out  : out std_logic_vector(7 downto 0)
    );
end eofgen;

architecture eofgen_architecture of eofgen is
  constant idle                          : std_logic_vector(15 downto 0)      := (others => '0');
  constant idle_eof                      : std_logic_vector(1 downto 0)      := "10";
  signal data_reg                        : t_std16_array(WORDS_PER_FIFO_WIDTH-1 downto 0);
  signal data_out_d                      : std_logic_vector(63 downto 0);
  signal dv_reg, in_packet               : std_logic;
  signal eof, eof_reg, end_of_fifo       : std_logic;
  signal width_match_counter             : std_logic_vector(1 downto 0);
begin

  
        
DATA_SHIFTS : for I in 1 to WORDS_PER_FIFO_WIDTH-1 generate
    process(CLK)
    begin
        if (RST = '1') then
            data_reg(I) <= (others => '0');
        elsif rising_edge(CLK) and (DV_IN = '1') then
            data_reg(I) <= data_reg(I-1);
        end if;
    end process;
end generate;

CLOCK_LOGIC : process(CLK, RST)
begin
    if (RST = '1') then
        data_reg(0)         <= (others => '0');
        width_match_counter <= (others => '0');
        in_packet           <= '0';
    elsif rising_edge(CLK) then
        if (DV_IN = '1') then
            data_reg(0) <= DATA_IN;
            width_match_counter <= width_match_counter + 1;
            in_packet   <= '1';
        else
            in_packet           <= '0';
        end if;
        if (eof = '1') then
            width_match_counter <= (others => '0');
        end if;
        dv_reg      <= DV_IN;
    end if;
end process;

-- use eof reg to reset width_match_counter


  eof <= not DV_IN and dv_reg;
  end_of_fifo <= '1' when (width_match_counter = b"00" and in_packet = '1') else '0';
  DV_OUT <= '1' when (eof or end_of_fifo) = '1' else '0';
  
  
    with width_match_counter select
        DATA_OUT <= data_reg(0) & idle & idle & idle
                        when b"01", 
                    data_reg(1) & data_reg(0) & idle & idle
                        when b"10",
                    data_reg(2) & data_reg(1) & data_reg(0) & idle              
                        when b"11",
                    data_reg(3) & data_reg(2) & data_reg(1) & data_reg(0)   
                        when others;
    with width_match_counter select
        EOF_OUT <=  eof & eof & idle_eof & idle_eof & idle_eof   
                        when b"01",
                    eof & eof & eof & eof & idle_eof & idle_eof                                                                                               
                        when b"10",
                    eof & eof & eof & eof & eof & eof & idle_eof              
                        when b"11",
                    eof & eof & eof & eof & eof & eof & eof & eof   
                        when others;
  
end eofgen_architecture;
