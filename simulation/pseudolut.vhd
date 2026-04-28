Library UNISIM;
library ieee;
use UNISIM.vcomponents.all;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

entity pseudolut is
  port (
    CLK   : in std_logic;
    ADDR  : in std_logic_vector(3 downto 0);
    DOUT1 : out std_logic_vector(15 downto 0);
    DOUT2 : out std_logic_vector(15 downto 0)
  );
end pseudolut;

architecture behavioral of pseudolut is
  type lut_array is array (0 to 15) of std_logic_vector(15 downto 0);

  --constant vme_addrs : lut_array := (x"4100", x"4200", x"4300", x"4100",
  --                                   x"401C", x"3200", x"4100", x"4200",
  --                                   x"4300", x"4100", x"4200", x"4300",
  --                                   x"4100", x"4200", x"4300", x"4100");
  --constant vme_datas : lut_array := (x"2EAD", x"2EAD", x"2EAD", x"2EAD",
  --                                   x"0000", x"0004", x"2EAD", x"2EAD",
  --                                   x"2EAD", x"2EAD", x"2EAD", x"2EAD",
  --                                   x"2EAD", x"2EAD", x"2EAD", x"2EAD");

  -- For DAQ
  constant vme_addrs : lut_array := (x"4100", x"4200", x"4300", x"4028",
                                     x"3300", x"3304", x"4000", x"4004",
                                     x"400C", x"401C", x"4100", x"4100",
                                     x"4300", x"4100", x"4200", x"4300");
  constant vme_datas : lut_array := (x"2EAD", x"2EAD", x"2EAD", x"0020",
                                     x"0001", x"0001", x"0026", x"0003",
                                     x"0021", x"0000", x"2EAD", x"2EAD",
                                     x"2EAD", x"2EAD", x"2EAD", x"2EAD");

  -- Does nothing
  --constant vme_addrs : lut_array := (x"4100", x"4200", x"4300", x"4100",
  --                                   x"4200", x"4300", x"4100", x"4200",
  --                                   x"4300", x"4100", x"4200", x"4300",
  --                                   x"4100", x"4200", x"4300", x"4100");
  --constant vme_datas : lut_array := (x"2EAD", x"2EAD", x"2EAD", x"2EAD",
  --                                   x"2EAD", x"2EAD", x"2EAD", x"2EAD",
  --                                   x"2EAD", x"2EAD", x"2EAD", x"2EAD",
  --                                   x"2EAD", x"2EAD", x"2EAD", x"2EAD");

  ---- SYSMON test: 7100 read SYSMON00
  --constant vme_addrs : lut_array := (x"4100", x"4200", x"4300", x"4100",
  --                                   x"4200", x"4300", x"4100", x"4200",
  --                                   x"7100", x"4100", x"4200", x"4300",
  --                                   x"4100", x"4200", x"4300", x"4100");
  --constant vme_datas : lut_array := (x"2EAD", x"2EAD", x"2EAD", x"2EAD",
  --                                   x"2EAD", x"2EAD", x"2EAD", x"2EAD",
  --                                   x"2EAD", x"2EAD", x"2EAD", x"2EAD",
  --                                   x"2EAD", x"2EAD", x"2EAD", x"2EAD");

  signal dout1_inner : std_logic_vector(15 downto 0) := (others => '0');
  signal dout2_inner : std_logic_vector(15 downto 0) := (others => '0');

begin 
   
   proc_pseudolut : process (CLK)
   begin
     if rising_edge(CLK) then
       dout1_inner <= vme_addrs(to_integer(unsigned(ADDR)));
       dout2_inner <= vme_datas(to_integer(unsigned(ADDR)));
     end if;
   end process;

   DOUT1 <= dout1_inner;
   DOUT2 <= dout2_inner;

end behavioral;

