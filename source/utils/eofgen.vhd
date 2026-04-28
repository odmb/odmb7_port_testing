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
    WORDS_PER_FIFO_WIDTH      : integer range 1 to 16 := 4; --! Shift reg depth
    DISCARD_DCFEB_24_BIT_DATA : boolean := true --! Whether to discard the first two words of DCFEB data that includes ALCT info
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
  signal word_cnt                        : integer range 0 to 100 := 0; -- counts to 100 since 100 words in DCFEB packet, used to discard first two words if DISCARD_DCFEB_24_BIT_DATA is true
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
        if (DISCARD_DCFEB_24_BIT_DATA = true) then
            word_cnt <= 0;
        else
            word_cnt <= 2; -- if not discarding, start at 2 since the first two words will be considered as part of the packet
        end if;
    elsif rising_edge(CLK) then
        if (DV_IN = '1') then
            word_cnt <= word_cnt + 1;
            if (word_cnt >= 2) then
                data_reg(0) <= DATA_IN;
                width_match_counter <= width_match_counter + 1;
                in_packet   <= '1';
            end if;
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
  
  -- Match DDU 64-bit format with first word received being lowest 16 bits
    with width_match_counter select
        DATA_OUT <= idle & idle & idle & data_reg(0)
                        when b"01", 
                    idle & idle & data_reg(1) & data_reg(1)
                        when b"10",
                    idle & data_reg(0) & data_reg(1) & data_reg(2)              
                        when b"11",
                    data_reg(0) & data_reg(1) & data_reg(2) & data_reg(3)   
                        when others;
    with width_match_counter select
        EOF_OUT <=  idle_eof & idle_eof & idle_eof & eof & eof
                        when b"01",
                    idle_eof & idle_eof & eof & eof & eof & eof                                                                                               
                        when b"10",
                    idle_eof & eof & eof & eof & eof & eof & eof
                        when b"11",
                    eof & eof & eof & eof & eof & eof & eof & eof   
                        when others;
  
end eofgen_architecture;
