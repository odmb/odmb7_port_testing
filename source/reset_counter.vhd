----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 03/02/2026 10:15:52 PM
-- Design Name: 
-- Module Name: reset_counter - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity reset_counter is
    Port ( clk : in STD_LOGIC;
           counter_enable : in STD_LOGIC;
           counter_reset : in STD_LOGIC;
           reset : in STD_LOGIC;
           count_out : out STD_LOGIC_VECTOR(15 downto 0));
end reset_counter;

architecture Behavioral of reset_counter is

signal count_unsigned : unsigned(15 downto 0) := (others => '0');

begin

proc_counter : process (clk)
  begin
    if(reset='1') then
        count_out <= (others => '0');
    elsif rising_edge(clk) then
      if counter_reset = '1' then
        count_unsigned <= (others => '0');
      elsif counter_enable = '1' then
        count_unsigned <= count_unsigned + 1;
      end if;
      
      count_out <= std_logic_vector(count_unsigned);
    end if;
  end process;



end Behavioral;
