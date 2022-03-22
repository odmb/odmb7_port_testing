-- COUNT_WINDOW: Counts number of CCs a signal is high in time window

library ieee;
library unisim;
use ieee.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use ieee.numeric_std.all;
use unisim.vcomponents.all;
use work.ucsb_types.all;

entity TIME_AVERAGE is
  generic (
    NCYCLES   : integer := 32;
    THRESHOLD : integer := 1
    );
  port (
    DOUT : out std_logic;               --! result of finding any 
    CLK  : in  std_logic;               --! clock for counting
    RST  : in  std_logic;               --! synchronized reset
    DIN  : in  std_logic                --! input signal
    );
end TIME_AVERAGE;

architecture TIME_AVERAGE_ARCH of TIME_AVERAGE is
  signal result  : std_logic := '0';
  signal nfound  : integer range 0 to NCYCLES-1;
  signal counter : integer range 0 to NCYCLES-1;
begin

  counter_proc : process (CLK, RST)
  begin
    if rising_edge(CLK) then
      if (RST = '1') then
        counter <= 1;
        nfound <= 0;
        result <= '0';
      else     
        counter <= counter + 1;
        if DIN = '1' then
          nfound <= nfound + 1;
        end if;
        if (nfound >= THRESHOLD) then
          result <= '1';
          DOUT <= '1';
        end if;
        if (counter = 0) then
          DOUT <= result;
          result <= '0';
          nfound <= 0;
        end if;
      end if;
    end if;
  end process;

end TIME_AVERAGE_ARCH;
