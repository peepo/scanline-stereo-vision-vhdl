library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.const.all;

entity BRAM_RF is
generic (N : integer);

port (
	clk: in std_logic;
	we: in std_logic;
	addr_a: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
	din: in std_logic_vector (N - 1 downto 0);
	dout: out std_logic_vector (N - 1 downto 0)
);
end BRAM_RF;

architecture Behavorial of BRAM_RF is
	type ram_type is array ( 0 to 2**ADDR_WIDTH - 1) of std_logic_vector(N - 1 downto 0);
	signal ram: ram_type;
begin
	process(clk)
	begin
		if (clk'event and clk = '1' ) then
			if (we = '1') then
				ram(to_integer(unsigned(addr_a))) <= din;
			end if;
			dout <= ram(to_integer(unsigned(addr_a)));
		end if;
	end process;
end Behavorial;