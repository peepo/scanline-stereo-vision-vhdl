library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use work.const.all;

entity Minimum is
	port(
		GlobalCost	: 	in GlobalCosts_array;
		disp		: 	out int_64
	);
end entity Minimum;

architecture Behavioral of Minimum is
	signal GC_32 : GlobalCosts_array_32;
	signal d_32	: disp_array_32;
	signal GC_16 : GlobalCosts_array_16;
	signal d_16	: disp_array_16;
	signal GC_8 : GlobalCosts_array_8;
	signal d_8	: disp_array_8;
	signal GC_4 : GlobalCosts_array_4;
	signal d_4	: disp_array_4;
	signal GC_2 : GlobalCosts_array_2;
	signal d_2	: disp_array_2;
	
	function comparator1(A: std_logic_vector(7 downto 0); B: std_logic_vector(7 downto 0)) return boolean is
	begin
		return A(7 downto 2) > B(7 downto 2);
	end comparator1;
begin

	process(GlobalCost) is
		variable d : int_64;
	begin
		d := 0;
		for k	in 0 to dmax -1 loop
			if(comparator1(GlobalCost(d), GlobalCost(k))) then
				d := k;
			end if;
		end loop;
		disp <= d;
	end process;
--M32_loop : for k in 0 to dmax/2 - 1 generate
--begin
--	process(GlobalCost(k), GlobalCost(k + 32)) is
--	begin
--		if(comparator1(GlobalCost(k), GlobalCost(k + 32))) then
--			GC_32(k) <= GlobalCost(k + 32);
--			d_32(k) <= k + 32;
--		else
--			GC_32(k) <= GlobalCost(k);
--			d_32(k) <= k;
--		end if;
--	end process;
--end generate M32_loop;
--
--M16_loop : for k in 0 to dmax/4 - 1 generate
--begin
--	process(GC_32(k*2), GC_32(k*2 + 1), d_32(k*2), d_32(k*2+1)) is
--	begin
--		if(comparator1(GC_32(k*2), GC_32(k*2 + 1))) then
--			GC_16(k) <= GC_32(k*2 + 1);
--			d_16(k) <= d_32(k*2);
--		else
--			GC_16(k) <= GC_32(k*2);
--			d_16(k) <= d_32(k*2 + 1);
--		end if;
--	end process;
--end generate M16_loop;
--
--M8_loop : for k in 0 to dmax/8 - 1 generate
--begin
--	process(GC_16(k*2), GC_16(k*2 + 1), d_16(k*2), d_16(k*2+1)) is
--	begin
--		if(comparator1(GC_16(k*2), GC_16(k*2 + 1))) then
--			GC_8(k) <= GC_16(k*2 + 1);
--			d_8(k) <= d_16(k*2);
--		else
--			GC_8(k) <= GC_16(k*2);
--			d_8(k) <= d_16(k*2 + 1);
--		end if;
--	end process;
--end generate M8_loop;
--
--M4_loop : for k in 0 to dmax/16 - 1 generate
--begin
--	process(GC_8(k*2), GC_8(k*2 + 1), d_8(k*2), d_8(k*2+1)) is
--	begin
--		if(comparator1(GC_8(k*2), GC_8(k*2 + 1))) then
--			GC_4(k) <= GC_8(k*2 + 1);
--			d_4(k) <= d_8(k*2);
--		else
--			GC_4(k) <= GC_8(k*2);
--			d_4(k) <= d_8(k*2 + 1);
--		end if;
--	end process;
--end generate M4_loop;
--
--M2_loop : for k in 0 to dmax/32 - 1 generate
--begin
--	process(GC_4(k*2), GC_4(k*2 + 1), d_4(k*2), d_4(k*2+1)) is
--	begin
--		if(comparator1(GC_4(k*2), GC_4(k*2 + 1))) then
--			GC_2(k) <= GC_4(k*2 + 1);
--			d_2(k) <= d_4(k*2);
--		else
--			GC_2(k) <= GC_4(k*2);
--			d_2(k) <= d_4(k*2 + 1);
--		end if;
--	end process;
--end generate M2_loop;
--
--process(GC_2(0), GC_2(1), d_2(0), d_2(1)) is
--begin
--	if(comparator1(GC_2(0), GC_2(1))) then
--		disp <= d_2(0);
--	else
--		disp <= d_2(1);
--	end if;
--end process;


end architecture Behavioral;