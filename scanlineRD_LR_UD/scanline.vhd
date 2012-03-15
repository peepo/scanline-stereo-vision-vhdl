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

	component Minimum is
	port (
		GlobalCost	: 	in GlobalCosts_array;
		disp		: 	out int_64
	);
	end component;


	component BRAM_RF is
	generic (N: integer);
	port (
		CLK		: in std_logic;
		WE			: in std_logic;
		ADDR_A	: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
		ADDR_B	: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
		DIN_A		: in std_logic_vector (N - 1 downto 0 );
		DOUT_B	: out std_logic_vector (N -1 downto 0 )
	);
	end component;

	signal ADDRRD_A	: Address_Width_array;
	signal ADDRRD_B	: Address_Width_array;
	signal DINRD_A		: GlobalCosts_array;
	signal DOUTRD_B	: GlobalCosts_array;

	signal ADDRDRD_A	: std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal ADDRDRD_B	: std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal DINDRD_A		: std_logic_vector (5 downto 0 );
	signal DOUTDRD_B	: std_logic_vector (5 downto 0 );
	
	signal ADDRUD_A	: Address_Width_array;
	signal ADDRUD_B	: Address_Width_array;
	signal DINUD_A		: GlobalCosts_array;
	signal DOUTUD_B	: GlobalCosts_array;

	signal ADDRD_A	: std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal ADDRD_B	: std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal DIND_A	: std_logic_vector (5 downto 0 );
	signal DOUTD_B	: std_logic_vector (5 downto 0 );

	
	function comparator(A: std_logic_vector(7 downto 0); B: std_logic_vector(7 downto 0)) return boolean is
	begin
		return A(7 downto 5) > B(7 downto 5);
	end comparator;
	
	function comparator1(A: std_logic_vector(7 downto 0); B: std_logic_vector(7 downto 0)) return boolean is
	begin
		return A(7 downto 2) > B(7 downto 2);
	end comparator1;

	signal LOCAL_COST 		: GlobalCosts_array;
	signal GLOBAL_COST 		: GlobalCosts_array;
	signal GLOBAL_COST3 		: GlobalCosts_array;
	signal GLOBAL_COST_PREV	: GlobalCosts_array;
	signal GLOBAL_COST_MIN	: GlobalCosts_array;
	signal MIN_OUT				: int_64;
	signal LINE_RIGHT 		: Line_array;
	signal LEFT_RL				: pixel;
	signal DD					: std_logic_vector(5 downto 0);
	signal I						: std_logic_vector(1 downto 0);
	signal J						: int_640;
	signal FIRST_LINE			: std_logic;
	signal GC00					: GC_array;
	signal GC01					: GC_array;
	signal GC1 					: GC_array;
	signal d0 					: int_64;
	signal d1 					: int_64;
	
begin

	min : Minimum
	port map(
		GlobalCost	=> GLOBAL_COST_MIN,
		disp		=> 	MIN_OUT
	);
	
	BRAM_loop : for k in 0 to dmax - 1 generate
	begin
		BramUD : BRAM_RF
		generic map (N => 8)
		port map(
			CLK		=> PIXEL_CLOCK,
			WE			=> '1',
			ADDR_A	=> ADDRUD_A(k),
			ADDR_B	=> ADDRUD_B(k),
			DIN_A		=> DINUD_A(k),
			DOUT_B	=> DOUTUD_B(k)
		);
		
		BramRD : BRAM_RF
		generic map (N => 8)
		port map(
			CLK		=> PIXEL_CLOCK,
			WE			=> '1',
			ADDR_A	=> ADDRRD_A(k),
			ADDR_B	=> ADDRRD_B(k),
			DIN_A		=> DINRD_A(k),
			DOUT_B	=> DOUTRD_B(k)
		);
	end generate BRAM_loop;

	BramLineDispUD : BRAM_RF
	generic map (N => 6)
	port map(
		CLK		=> PIXEL_CLOCK,
		WE			=> '1',
		ADDR_A	=> ADDRD_A,
		ADDR_B	=> ADDRD_B,
		DIN_A		=> DIND_A,
		DOUT_B	=> DOUTD_B 
	);

	BramLineDispRD : BRAM_RF
	generic map (N => 6)
	port map(
		CLK		=> PIXEL_CLOCK,
		WE			=>'1',
		ADDR_A	=> ADDRDRD_A,
		ADDR_B	=> ADDRDRD_B,
		DIN_A		=> DINDRD_A,
		DOUT_B	=> DOUTDRD_B 
	);


ID : process(PIXEL_CLOCK) is
	variable LineRightLR : Line_array;
begin
	if PIXEL_CLOCK = '1' and PIXEL_CLOCK'EVENT then
		if RESET = '1' or FRAME_VALID_IN = '0' then
			FRAME_VALID_OUT <= '0';
--			LINE_VALID_OUT <= '0';
			j <= 0;
			LINE_RIGHT	<= (others =>(others => '0'));
			LEFT_RL		<= (others => '0');
			FIRST_LINE	<= '1';
			LineRightLR := (others =>(others => '0'));
			
			ADDRRD_B <=(others =>	conv_std_logic_vector(0,ADDR_WIDTH));
			ADDRRD_A <=(others =>	conv_std_logic_vector(0,ADDR_WIDTH));			
			ADDRDRD_A	<=	conv_std_logic_vector(0, ADDR_WIDTH);					
			ADDRDRD_B	<=	conv_std_logic_vector(0, ADDR_WIDTH);					

			ADDRUD_B <=(others =>  conv_std_logic_vector(0,ADDR_WIDTH));
			ADDRUD_A <=(others => conv_std_logic_vector(0,ADDR_WIDTH));
			ADDRD_B	<=conv_std_logic_vector(0, ADDR_WIDTH);
			ADDRD_A	<=conv_std_logic_vector(0, ADDR_WIDTH);
			
		else
			if FRAME_VALID_IN = '1' then
				FRAME_VALID_OUT <= '1';
				if LINE_VALID_IN = '1'  then
					LineRightLR := LineRightLR(dmax - 2 downto 0) & RIGHT;
					LINE_RIGHT <= LineRightLR;
					LEFT_RL <= LEFT;
					j <= j + 1;
					
					ADDRUD_B <=(others =>  conv_std_logic_vector(j + 1,ADDR_WIDTH));
					ADDRUD_A <=(others => conv_std_logic_vector(j - 1,ADDR_WIDTH));
					
					ADDRD_B <=conv_std_logic_vector(j + 1, ADDR_WIDTH);
					ADDRD_A <=conv_std_logic_vector(j - 1, ADDR_WIDTH);


					ADDRRD_B <=(others =>	conv_std_logic_vector(j + 2,ADDR_WIDTH));
					ADDRRD_A <=(others =>	conv_std_logic_vector(j - 1,ADDR_WIDTH));
					
					ADDRDRD_A	<=	conv_std_logic_vector(j - 1, ADDR_WIDTH);					
					ADDRDRD_B	<=	conv_std_logic_vector(j + 2, ADDR_WIDTH);

					if first_line = '1' then
						ADDRUD_A <=(others => conv_std_logic_vector(j + 1,ADDR_WIDTH));
						ADDRD_A <=conv_std_logic_vector(j + 1, ADDR_WIDTH);
						ADDRRD_A <=(others =>	conv_std_logic_vector(j + 2,ADDR_WIDTH));
						ADDRDRD_A	<=	conv_std_logic_vector(j + 2, ADDR_WIDTH);
					end if;
					
				else
					if j = Width then
						FIRST_LINE <= '0';
					end if;
					j <= 0;
				end if;
			else
				FRAME_VALID_OUT <= '0';
			end if;
		end if; --RESET			
	end if; -- PIXEL CLOCK
end process ID;

LOCAL_EX_loop : for k in 0 to dmax-1 generate
begin
	LOCAL_EX : process(PIPELINE_CLOCK) is
		variable LocalCost : LP_element;
	begin
		if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT then
			if RESET = '1' or FRAME_VALID_IN = '0' then
				LOCAL_COST(k) <= (others => '0');
			elsif LINE_VALID_IN = '1'  then
				if (LEFT_RL > LINE_RIGHT(k) )then
					LOCAL_COST(k) <= (LEFT_RL - LINE_RIGHT(k));
				else
					LOCAL_COST(k) <= (LINE_RIGHT(k) - LEFT_RL);
				end if;
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
			if RESET = '1' or FRAME_VALID_IN = '0' then
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
		if RESET = '1' or FRAME_VALID_IN = '0' then
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
		if RESET = '1' or FRAME_VALID_IN = '0' then
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



DISP3 : process(PIPELINE_CLOCK) is
	variable GlobalCost3 : GlobalCosts_array;
	variable GlobalCost_disp : 			GlobalCosts_array_disp;
	variable GlobalCost_disp3 : 			GlobalCosts_array_disp;
begin
	if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
		if RESET = '1' or FRAME_VALID_IN = '0' then
			GlobalCost3		:= (others =>(others => '0'));
			GlobalCost_disp := (others =>(others =>(others => '0')));
			GlobalCost_disp3 := (others =>(others =>(others => '0')));
			GLOBAL_COST_PREV <= (others => (others => '0'));
			GLOBAL_COST3 <= (others => (others => '0'));

			DINRD_A 	<= (others =>(others => '0'));
			DINUD_A <=  (others =>(others => '0'));
		elsif LINE_VALID_IN = '1' then		
				
				if first_line = '0' then
					GlobalCost_disp3(3) := DOUTUD_B;		
					GlobalCost_disp3(1) := DOUTRD_B;
				end if;
		
				GlobalCost_disp(conv_integer(i)) := GLOBAL_COST;

				DINUD_A <= GlobalCost_disp(1);
				DINRD_A <= GlobalCost_disp(3);
				GlobalCost_disp3(2) := GlobalCost_disp(0);				
				GLOBAL_COST_PREV <= GlobalCost_disp3(conv_integer(i));
				if i = "10" then
					GlobalCost3 := (others =>(others => '0'));
				end if;
				for k in 0 to dmax - 1 loop
					GlobalCost3(k) := GlobalCost3(k) + GlobalCost_disp3(conv_integer(i + 2))(k)(7 downto 2);
				end loop;
				GLOBAL_COST3 <= GlobalCost3;
		end if;
	end if;
end process DISP3;

--DISP0 : process(PIPELINE_CLOCK) is	
--	variable GlobalCost5 : GlobalCosts_array;
--begin
--	if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
--		if RESET = '1' or FRAME_VALID_IN = '0' then
--			GlobalCost5		:= (others =>(others => '0'));
--		elsif LINE_VALID_IN = '1' then
--			if(i = "10") then
--				GlobalCost5 := GLOBAL_COST3;
--			else
--				GlobalCost5 := GLOBAL_COST;
--			end if;
--
--			d := 0;
--			for k in 0 to  (dmax - 1)/3  loop
--				if(comparator1(GlobalCost5(d), GlobalCost5(k))) then
--					d := k;
--				end if;
--			end loop;
--		end if;
--	end if;
--end process DISP0;

 
DISP : process(PIPELINE_CLOCK) is	
	variable d	: int_64;
	variable GlobalCost5					: GlobalCosts_array;
	variable GlobalCost_disp			: GlobalCosts_array_disp;
	variable GlobalCost_disp3			: GlobalCosts_array_disp;
	variable d_disp						: d_disp_array;
	variable d_disp3						: d_disp_array;
begin
	if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
		if RESET = '1' or FRAME_VALID_IN = '0' then
			LINE_VALID_OUT <= '0';
			
			dd <= (others => '0');			
			GlobalCost5			:= (others =>(others => '0'));
			GLOBAL_COST_MIN	<= (others =>(others => '0'));
			i <= "00";
						
			d_disp := (others => 0);
			d_disp3 := (others => 0);
						
			DINDRD_A	<=	(others => '0');
			DIND_A	<= (others => '0');
			
			GC00	<= (others =>(others => '0'));
			GC01	<= (others =>(others => '0'));
			GC1	<= (others =>(others => '0'));
			d0		<= 0;
			d1		<= 0;
			

		elsif LINE_VALID_IN = '1'    then
				LINE_VALID_OUT <= '1';
				
--				if(i = "10") then
--					GlobalCost5 := GLOBAL_COST3;
--				else
--					GlobalCost5 := GLOBAL_COST;
--				end if;
			
			if(i = "10") then
					GLOBAL_COST_MIN <= GLOBAL_COST3;
				else
					GLOBAL_COST_MIN <= GLOBAL_COST;
				end if;
			
			

--				d := 0;
--				for k in 0 to  (dmax - 1) / 3 loop
--					if(comparator1(GlobalCost5(d), GlobalCost5(k))) then
--						d := k;
--					end if;
--				end loop;
--				d0 <= d;
--				
--				for k in 0 to  (dmax - 1) / 3 - 1 loop
--					GC00(k) <=  GlobalCost5(k + (dmax - 1) / 3 + 1);
--					GC01(k) <=  GlobalCost5(k + 2 * (dmax - 1) / 3 + 1);				
--				end loop;
--				
--				GC00((dmax - 1) / 3) <= GlobalCost5(d);
--				
--				d := 0;
--				for k in 0 to (dmax - 1)/3 loop
--					if(comparator1(GC00(d), GC00(k))) then
--						d := k;
--					end if;
--				end loop;
--				
--				GC1 <= GC01;
--				GC1((dmax - 1) / 3) <= GC00(d);
--				
--				if d = (dmax - 1)/3 then
--					d1 <= d0;
--				else
--					d1 <= (dmax - 1) / 3 + 1 + d;
--				end if;
--						
--				
--				d := 0;
--				for k in 0 to  (dmax - 1)/3 loop
--					if(comparator1(GC1(d), GC1(k))) then
--						d := k;
--					end if;
--				end loop;
--
--				if d = (dmax - 1)/3 then
--					d := d1;
--				else
--					d := 2 * (dmax - 1) / 3 + 1 + d;
--				end if;
--
--
--


				d := MIN_OUT;
--				d := 0;
--				for k in 0 to  dmax - 1 loop
--					if(comparator1(GlobalCost5(d), GlobalCost5(k))) then
--						d := k;
--					end if;
--				end loop;


				
				d_disp(conv_integer(i - 1)) := d;		-- 00 LR // 01 UD // 10 OUT // 11 RD	--// LR //UD//  OUT// RD
				
				DIND_A <= conv_std_logic_vector(d_disp(1), 6);
				DATA_OUT <= conv_std_logic_vector(d_disp(2), 4) & "0000";
				DINDRD_A	<=	conv_std_logic_vector(d_disp(3), 6);
				
				d_disp3(0) := 0;
				d_disp3(2) := d_disp(0);
				
				if first_line = '1' then
					d_disp3(3) := 0;
					d_disp3(1) := 0;
				else
					d_disp3(3) := conv_integer(DOUTD_B);
					d_disp3(1) := conv_integer(DOUTDRD_B);
				end if;
		
				dd <= conv_std_logic_vector(d_disp3(conv_integer(i)), 6); 	-- //00 0//01 RD//10 LR//11 UD 
				
				i<= i + 1;
		else
			LINE_VALID_OUT <= '0';			
		end if;	--line valid
	end if;	--reset
end process DISP;

end Behavioral;