Library UNISIM;
library ieee;
use UNISIM.vcomponents.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity pseudolut is
  generic (
    CMDSET : integer := 0   --! 0: DAQ test 1: SPI test 2: CFEBJTAG test
    );
  port (
    CLK   : in std_logic;
    ADDR  : in integer;
    DOUT1 : out std_logic_vector(15 downto 0);
    DOUT2 : out std_logic_vector(15 downto 0)
  );
end pseudolut;

architecture behavioral of pseudolut is
  type lut_array is array (0 to 15) of std_logic_vector(15 downto 0);

  constant addr_daq : lut_array := (x"4100", x"4200", x"4300", x"4028",
                                    x"3300", x"3304", x"4000", x"4004",
                                    x"400C", x"401C", x"3200", x"4100",
                                    x"4300", x"4100", x"4200", x"4300");
  constant data_daq : lut_array := (x"2EAD", x"2EAD", x"2EAD", x"0020",
                                    x"0001", x"0001", x"0026", x"0003",
                                    x"0021", x"0000", x"0004", x"2EAD",
                                    x"2EAD", x"2EAD", x"2EAD", x"2EAD");

  constant addr_dev1 : lut_array := (x"4100", x"4200", x"4300", x"4100",
                                     x"4200", x"4300", x"4100", x"4200",
                                     x"1018", x"1020", x"1024", x"191C",
                                     x"1F04", x"1014", x"1F08", x"1014");
  constant data_dev1 : lut_array := (x"2EAD", x"2EAD", x"2EAD", x"2EAD",
                                     x"2EAD", x"2EAD", x"2EAD", x"2EAD",
                                     x"0000", x"0002", x"2EAD", x"03C9",
                                     x"0000", x"2EAD", x"0000", x"2EAD");

  constant addr_spi : lut_array := (x"4100", x"602C", x"602C", x"602C",
                                    x"602C", x"602C", x"6030", x"4100",
                                    x"4200", x"4300", x"4200", x"4200",
                                    x"4300", x"4100", x"4200", x"4300");
  constant data_spi : lut_array := (x"2EAD", x"1FF7", x"0001", x"0033",
                                    x"0034", x"0035", x"2EAD", x"2EAD",
                                    x"2EAD", x"2EAD", x"2EAD", x"2EAD",
                                    x"2EAD", x"2EAD", x"2EAD", x"2EAD");
  
  signal dout1_inner : std_logic_vector(15 downto 0) := (others => '0');
  signal dout2_inner : std_logic_vector(15 downto 0) := (others => '0');

begin 
   
  proc_pseudolut : process (CLK)
  begin
    if rising_edge(CLK) then
      case CMDSET is  
        when 0 =>
          dout1_inner <= addr_daq(ADDR);
          dout2_inner <= data_daq(ADDR);
        when 1 =>
          dout1_inner <= addr_spi(ADDR);
          dout2_inner <= data_spi(ADDR);
        when 2 =>
          dout1_inner <= addr_dev1(ADDR);
          dout2_inner <= data_dev1(ADDR);
        when others =>
          dout1_inner <= addr_daq(ADDR);
          dout2_inner <= data_daq(ADDR);
      end case;
    end if;
  end process;

  DOUT1 <= dout1_inner;
  DOUT2 <= dout2_inner;

end behavioral;

