library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
use work.const.all;

entity scanline is
	Port (  
		RESET 				: in  std_logic;
		FRAME_VALID_IN 		: in  std_logic;
		LINE_VALID_IN 		: in  std_logic;
		LEFT 				: in  std_logic_vector (7 downto 0);
		RIGHT 				: in  std_logic_vector (7 downto 0);
		PIXEL_CLOCK			: in  std_logic;    
		PIPELINE_CLOCK		: in  std_logic;    
		DATA_OUT 			: out  std_logic_vector (7 downto 0);
		FRAME_VALID_OUT 	: out  std_logic;
		LINE_VALID_OUT 		: out  std_logic
	);
end scanline;


architecture Behavioral of scanline is

	function comparator(A: std_logic_vector(7 downto 0); B: std_logic_vector(7 downto 0)) return boolean is
	begin
		return A(7 downto 0) > B(7 downto 0);
	end comparator;
	
	function comparator1(A: std_logic_vector(7 downto 0); B: std_logic_vector(7 downto 0)) return boolean is
	begin
		return A(7 downto 0) > B(7 downto 0);
	end comparator1;

	signal LOCAL_COST 		: GlobalCosts_array;
	signal GLOBAL_COST 		: GlobalCosts_array;
	signal GLOBAL_COST_PREV	: GlobalCosts_array;
	signal LINE_RIGHT 		: Line_array;
	signal LEFT_RL			: pixel;
	signal dd				: std_logic_vector(5 downto 0);
	signal i				: std_logic_vector(1 downto 0);
	signal b				: std_logic;
	signal j				: integer := 0;

	signal dpl0			: d_pl0_array;
	signal dpl1			: d_pl1_array;
	signal GC0			: GC0_array;
	signal GC1			: GC1_array;
	signal dpl0v			: d_pl0_array;
	signal dpl1v			: d_pl1_array;
	signal GC0v			: GC0_array;
	signal GC1v			: GC1_array;
begin


ID : process(PIXEL_CLOCK) is
begin
	if PIXEL_CLOCK = '1' and PIXEL_CLOCK'EVENT then
		if RESET = '1' then
			FRAME_VALID_OUT <= '0';
			LINE_VALID_OUT <= '0';
			j <= 0;
			b <= '0';
		else
			if FRAME_VALID_IN = '1' then
				FRAME_VALID_OUT <= '1';
				if LINE_VALID_IN = '1' then
					LINE_VALID_OUT <= '1';					
					j <= j + 1;
				else
					if j = Width then
						b <= not b;
					end if;
					LINE_VALID_OUT <= '0';					
					j <= 0;
				end if;
			else
				FRAME_VALID_OUT <= '0';
			end if;
		end if; --RESET			
	end if; -- PIXEL CLOCK
end process ID;

F : process(PIPELINE_CLOCK) is
	variable LineRightLR : Line_array;
begin

	if(PIPELINE_CLOCK'EVENT and PIPELINE_CLOCK = '1') then
		if RESET = '1' then
			LEFT_RL		<= (others => '0');
			LINE_RIGHT  <= (others =>(others => '0'));
			LineRightLR := (others =>(others => '0'));
			i <= "00";	
		elsif LINE_VALID_IN = '1' then
			if(i = "00") then
				LEFT_RL		<= (others => '0');
				LINE_RIGHT  <= (others =>(others => '0'));
			elsif(i = "01") then
				LEFT_RL		<= (others => '0');
				LINE_RIGHT  <= (others =>(others => '0'));
			elsif(i = "10") then
				LineRightLR := LineRightLR(dmax - 2 downto 0) & RIGHT;
				LINE_RIGHT <= LineRightLR;
				LEFT_RL <= LEFT;
			elsif(i = "11") then
				LEFT_RL		<= (others => '0');
				LINE_RIGHT  <= (others =>(others => '0'));
			end if;
				i <= i + 1;
		end if;
	end if;
end process F;

LOCAL_EX_loop : for k in 0 to dmax-1 generate
begin
	LOCAL_EX : process(PIPELINE_CLOCK) is
		variable LocalCost : LP_element;
	begin
		if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT then
			if RESET = '1' then
				LocalCost := (others => '0');
				LOCAL_COST(k) <= (others => '0');
			elsif LINE_VALID_IN = '1' then
--					LocalCost := (abs( signed(LEFT_RL) - signed(LINE_RIGHT(k)) ));
				if (LEFT_RL > LINE_RIGHT(k) )then
					LocalCost := (LEFT_RL - LINE_RIGHT(k));
				else
					LocalCost := (LINE_RIGHT(k) - LEFT_RL);
				end if;
				if LocalCost = "XXXXXXXX" then
					LocalCost := (others => '0');
				end if;
				LOCAL_COST(k) <= LocalCost;
			end if;
		end if;	
	end process LOCAL_EX;
end generate LOCAL_EX_loop;

GLOBAL_EX_loop : for k in 1 to dmax-2 generate
begin	
	EX : process(PIPELINE_CLOCK) is
		variable LP : LP_element ;
		variable nn : int_64;
	begin
		if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT then
			if RESET = '1' then
				GLOBAL_COST(k) <= (others => '0');
				nn := 0;
				LP := (others => '0');
			elsif LINE_VALID_IN = '1' then
				nn := k - 1;
				LP := GLOBAL_COST_PREV(k);
				if(comparator(GLOBAL_COST_PREV(k - 1),GLOBAL_COST_PREV(k + 1))) then
					nn := k + 1;
				end if;
				if(comparator(GLOBAL_COST_PREV(k),GLOBAL_COST_PREV(nn) + P1)) then
					LP := GLOBAL_COST_PREV(nn) + P1;
				end if;		
				if(comparator(LP,P2 + GLOBAL_COST_PREV(conv_integer(dd)))) then
					LP := P2 + GLOBAL_COST_PREV(conv_integer(dd));
				end if;
				GLOBAL_COST(k) <= LOCAL_COST(k) + LP - GLOBAL_COST_PREV(conv_integer(dd));
			end if; --RESET		
		end if; --PIXEL_CLOCK
	end process EX;
end generate GLOBAL_EX_loop;

EX0 : process(PIPELINE_CLOCK) is
	variable LP : LP_element := (others => '0');
begin
	if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
		if RESET = '1' then
				GLOBAL_COST(0) <= (others => '0');
				LP := (others => '0');				
		elsif LINE_VALID_IN = '1'  then
			LP := GLOBAL_COST_PREV(0);
			if(comparator(LP,GLOBAL_COST_PREV(1) + P1)) then
				LP := GLOBAL_COST_PREV(1) + P1;
			end if;				
			if(comparator(LP,P2 + GLOBAL_COST_PREV(conv_integer(dd)))) then
				LP := P2 + GLOBAL_COST_PREV(conv_integer(dd));
			end if;
			GLOBAL_COST(0) <= LOCAL_COST(0) + LP - GLOBAL_COST_PREV(conv_integer(dd));
		end if; --RESET		
	end if; --PIXEL_CLOCK
end process EX0;

EXDmax : process(PIPELINE_CLOCK) is
	variable LP : LP_element := (others => '0');
begin
	if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
		if RESET = '1' then
			LP := (others => '0');
			GLOBAL_COST(dmax - 1) <= (others => '0');
		elsif LINE_VALID_IN = '1'  then
			LP := GLOBAL_COST_PREV(dmax - 1);
			if(comparator(GLOBAL_COST_PREV(dmax - 1),GLOBAL_COST_PREV(dmax - 2) + P1)) then
				LP := GLOBAL_COST_PREV(dmax - 2) + P1;
			end if;				
			if(comparator(LP,P2 + GLOBAL_COST_PREV(conv_integer(dd)))) then
				LP := P2 + GLOBAL_COST_PREV(conv_integer(dd));
			end if;
			GLOBAL_COST(dmax - 1) <= LOCAL_COST(dmax - 1) + LP - GLOBAL_COST_PREV(conv_integer(dd));
		end if; --RESET		
	end if; --PIXEL_CLOCK
end process EXDmax;

PL0 : for k in 0 to dmax/4 - 1 generate
begin
	DPL00 : process(PIPELINE_CLOCK) is	
	variable d	: int_64;
	begin
		if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
			if RESET = '1' then
				dpl0(k)	<= 0;
				GC0(k)	<= (others => '0');
			elsif LINE_VALID_IN = '1'  then	
				d := k * 4;
				for z in k * 4  to (k + 1) * 4 - 1 loop
					if(comparator1(GLOBAL_COST(d), GLOBAL_COST(z))) then
						d := z;
					end if;
				end loop;				
				dpl0(k)	<= d;
				GC0(k)		<= GLOBAL_COST(d);
			end if;
		end if;	
	end process DPL00;
end generate PL0;

PL1 : for k in 0 to dmax/16 - 1 generate
begin
	DPL01 : process(PIPELINE_CLOCK) is	
	variable d	: int_64;
	begin
		if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
			if RESET = '1' then
				dpl1(k)	<= 0;
				GC1(k)	<= (others => '0');
			elsif LINE_VALID_IN = '1'  then					
				d := k * 4;
				for z in k * 4 to (k + 1) * 4 - 1 loop
					if(comparator1(GC0(d), GC0(z))) then
						d := z;
					end if;
				end loop;				
				dpl1(k)	<= dpl0(d);
				GC1(k)	<= GC0(d);							
			end if;
		end if;		
	end process DPL01;
end generate PL1;


DISP : process(PIPELINE_CLOCK) is	
	variable d		: int_64;
	variable d0		: int_64;
	variable d1		: int_64;
	variable d2		: int_64;
	variable d3		: int_64;
	variable dLR	: int_64;
	variable GlobalCostLR : GlobalCosts_array;
	
--	variable dpl0v			: d_pl0_array;
--	variable dpl1v			: d_pl1_array;
--	variable GC0v			: GC0_array;
--	variable GC1v			: GC1_array;
begin
	if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
		if RESET = '1' then
			GLOBAL_COST_PREV <= (others =>(others => '0'));
			d := 0;
			dLR := 0;
			dd <= (others => '0');
			
			GC0v <= (others =>(others => '0'));
			GC1v <= (others =>(others => '0'));
			dpl0v <= (others => 0);
			dpl1v <= (others => 0);
		elsif LINE_VALID_IN = '1'  then	
			
--			d1 := 0;
--			d2 := 0;
--			d3 := 0;
			
--			d := 0;
--			for k in 0 to dmax - 1 loop
--				if(comparator1(GLOBAL_COST(d), GLOBAL_COST(k))) then
--					d := k;
--				end if;
--			end loop;

--			for k in 0 to dmax / 4 - 1 loop
--				d := k * 4;
--				for z in k * 4  to (k + 1) * 4 - 1 loop
--					if(comparator1(GLOBAL_COST(d), GLOBAL_COST(z))) then
--						d := z;
--					end if;
--				end loop;				
--				dpl0v(k)	<= d;
--				GC0v(k)		<= GLOBAL_COST(d);
--			end loop;
--			
--			for k in 0 to dmax / 16 - 1 loop
--				d := k * 4;
--				for z in k * 4 to (k + 1) * 4 - 1 loop
--					if(comparator1(GC0v(d), GC0v(z))) then
--						d := z;
--					end if;
--				end loop;				
--				dpl1v(k)	<= dpl0v(d);
--				GC1v(k)		<= GC0v(d);			
--			end loop;



--			d := 0;
--			for k in 0 to dmax - 1 loop
--				if(comparator1(GLOBAL_COST(d), GLOBAL_COST(k))) then
--					d := k;
--				end if;
--			end loop;

--			d := dpl1v(0);
			


--			
--			d0 := 0 * 4;
--			for k in 0 to dmax / 4 - 1 loop
--				if(comparator1(GLOBAL_COST(d0), GLOBAL_COST(k))) then
--					d0 := k;
--				end if;
--			end loop;
--			
--			d1 := 1 * 4;
--			for k in dmax / 4 to 2*dmax / 4 - 1 loop
--				if(comparator1(GLOBAL_COST(d1), GLOBAL_COST(k))) then
--					d1 := k;
--				end if;
--			end loop;
--
--			d2 := 2 * 4;
--			for k in 2*dmax / 4 to 3*dmax / 4 - 1 loop
--				if(comparator1(GLOBAL_COST(d2), GLOBAL_COST(k))) then
--					d2 := k;
--				end if;
--			end loop;
--
--			d3 := 3 * 4;
--			for k in 3*dmax / 4 to 4*dmax / 4 - 1 loop
--				if(comparator1(GLOBAL_COST(d3), GLOBAL_COST(k))) then
--					d3 := k;
--				end if;
--			end loop;
--
--
--			d:= d3;
--			if(comparator1(GLOBAL_COST(d3), GLOBAL_COST(d2))) then
--				d := d2;
--			end if;
--			if(comparator1(GLOBAL_COST(d), GLOBAL_COST(d1))) then
--				d := d1;
--			end if;
--			if(comparator1(GLOBAL_COST(d), GLOBAL_COST(d1))) then
--				d := d1;
--			end if;

			if i = "00" then
				GLOBAL_COST_PREV <= (others =>(others => '1'));			
			elsif i = "01" then
				
				GlobalCostLR := GLOBAL_COST;
				
--				dLR := d;
				GLOBAL_COST_PREV <= (others =>(others => '1'));
			elsif i = "10" then
				GLOBAL_COST_PREV <= (others =>(others => '0'));
				dd <= conv_std_logic_vector(8, 6);
			elsif i= "11" then
				GLOBAL_COST_PREV <= GlobalCostLR;
--				dd <= conv_std_logic_vector(dLR, 6);
--				dLR := dpl1v(0);
				dLR := dpl1(0);
				DATA_OUT <= conv_std_logic_vector(dLR , 4) & "0000";				
				dd <= conv_std_logic_vector(dLR, 6);
--				d := 0;
--				for z in 0 to dmax / 16 - 1 loop
--					if(comparator1(GC1(d), GC1(z))) then
--						d := z;
--					end if;
--				end loop;
--				d := dpl1(d);
--				DATA_OUT <= conv_std_logic_vector(d , 5) & "000";				
--				dd <= conv_std_logic_vector(d, 6);

--				if dpl1(0) = dpl1v(0) then
--					report "1111111111111111";
--				else
--					report "0000000000000000";
--				end if;
			end if;
		end if;
	end if;
end process DISP;
 
end Behavioral;