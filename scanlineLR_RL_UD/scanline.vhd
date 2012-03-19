library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
--use IEEE.numeric_std.all;
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

	component BRAM_RF is
	generic (N : integer);
	port (
		clk: in std_logic;
		we: in std_logic;
		addr_a: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
		din: in std_logic_vector (N - 1 downto 0 );
		dout: out std_logic_vector (N - 1 downto 0 )
	);
	end component;
	
	component BRAM_WF is	
	generic (N : integer);
	port (
		clk: in std_logic;
		we: in std_logic;
		addr_a: in std_logic_vector		(ADDR_WIDTH - 1 downto 0);
		addr_b: in std_logic_vector		(ADDR_WIDTH - 1 downto 0);
		din_a: in std_logic_vector		(N - 1 downto 0 );
		dout_b: out std_logic_vector	(N -1 downto 0 )
	);
	end component;
	
	signal addrUD_a	: Address_Width_array;
	signal addrUD_b	: Address_Width_array;
	signal dinUD_A	: GlobalCosts_array;
	signal doutUD_B	: GlobalCosts_array;

	signal addrD_a : std_logic_vector	(ADDR_WIDTH - 1 downto 0);
	signal addrD_b : std_logic_vector	(ADDR_WIDTH - 1 downto 0);
	signal dinD_a : std_logic_vector	(5 downto 0 );
	signal doutD_b : std_logic_vector	(5 downto 0 );

	signal addrL_a: std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal addrL_b: std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal dinL :  std_logic_vector (7 downto 0 );
	signal doutL : std_logic_vector (7 downto 0 );
	
	signal addrR_a : std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal addrR_b : std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal dinR : std_logic_vector (7 downto 0 );
	signal doutR : std_logic_vector (7 downto 0 );

	signal addrD : std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal dinD : std_logic_vector (5 downto 0 );
	signal doutD : std_logic_vector (5 downto 0 );
	
	signal addrLR	: Address_Width_array;
	signal dinLR	: GlobalCosts_array;
	signal doutLR	: GlobalCosts_array;
	
	
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
	signal GLOBAL_COST_PREV	: GlobalCosts_array;
	signal LINE_RIGHT 		: Line_array;
	signal LEFT_RL			: pixel;
	signal dd				: std_logic_vector(5 downto 0);
	signal i				: std_logic_vector(1 downto 0);
	signal b				: std_logic;
	signal j				: int_640;
	signal z				: int_640;
	signal LineRight0		: Line_array;
	signal LineRight1		: Line_array;
	signal RL_Line			: RL_Line_array;
	signal FIRST_LINE		: std_logic_vector(1 downto 0);

	signal GLOBAL_COST_TRE  : GlobalCosts_array;
	signal DPL0             : d_pl0_array;
	signal DPL1             : d_pl1_array;
	signal GC0              : GC0_array;
	signal GC1              : GC1_array;

	
begin


BRAM_loop : for k in 0 to dmax-1 generate
begin
	BramLineGlobalCostUD : BRAM_WF
		generic map (N => DATA_WIDTH)
		port map(
			CLK		=> PIXEL_CLOCK,
			WE		=> '1',
			ADDR_A	=> ADDRUD_A(k),
			ADDR_B	=> ADDRUD_B(k),
			DIN_A	=> DINUD_A(k),
			DOUT_B	=> DOUTUD_B(k)
		);
	
	BramLineGlobalCostSum : BRAM_RF
		generic map (N => DATA_WIDTH)
		port map(
			CLK		=> PIXEL_CLOCK,
			WE		=> '1',
			ADDR_A	=> ADDRLR(k),
			DIN		=> DINLR(k),
			DOUT	=> DOUTLR(k)
		);
end generate BRAM_loop;

BramLineDispUD : BRAM_WF
	generic map (N => 6)
	port map(
		clk => PIXEL_CLOCK,
		we =>'1',
		addr_a => addrD_a,
		addr_b => addrD_b,
		din_a=> dinD_a,
		dout_b=> doutD_b 
	);


BramLineLeft : BRAM_RF
	generic map (N => 8)
	port map(
		clk => PIXEL_CLOCK,
		we =>'1',
		addr_a => addrL_a,
		din=> dinL ,
		dout=> doutL 
	);

BramLineRight : BRAM_WF
	generic map (N => 8)
	port map(
		clk => PIXEL_CLOCK,
		we => '1',
		addr_a => addrR_a,
		addr_b => addrR_b,
		din_a => dinR ,
		dout_b => doutR 
	);
		
BramLineDisp : BRAM_RF
	generic map (N => 6)
	port map(
		clk => PIXEL_CLOCK,
		we =>'1',
		addr_a => addrD,
		din=> dinD,
		dout=> doutD 
	);


ID : process(PIXEL_CLOCK) is

begin
	if PIXEL_CLOCK = '1' and PIXEL_CLOCK'EVENT then
		if RESET = '1' then
			FRAME_VALID_OUT <= '0';
			LINE_VALID_OUT <= '0';
			j <= 0;
			z <= 0;
			b <= '0';
			FIRST_LINE <= "00";
			
			ADDRUD_B <= (others =>(others => '0'));
			ADDRUD_A <= (others =>(others => '0'));
			ADDRD_A	<=(others => '0');
			ADDRD_B <=(others => '0');
			
			addrD <= (others => '0');
			ADDRLR <= (others =>(others => '0'));

		else
			if FRAME_VALID_IN = '1' then
				FRAME_VALID_OUT <= '1';
				if LINE_VALID_IN = '1' then
					LINE_VALID_OUT <= '1';					
					j <= j + 1;
					
					if b = '0' then
						z <= j;
					else
						z <= Width - j - 1;
					end if;

					addrD <= conv_std_logic_vector(z, ADDR_WIDTH);
					
					ADDRLR <= (others => conv_std_logic_vector(z, ADDR_WIDTH));
					
					ADDRUD_B <= (others => conv_std_logic_vector(j + 1,ADDR_WIDTH));
					ADDRUD_A <= (others => conv_std_logic_vector(j - 1,ADDR_WIDTH));
					ADDRD_A	<=conv_std_logic_vector(j - 1, ADDR_WIDTH);
					ADDRD_B <=conv_std_logic_vector(j + 1, ADDR_WIDTH);
					
					if first_line = "00" then
						ADDRUD_A <=(others => conv_std_logic_vector(j + 1,ADDR_WIDTH));
						ADDRD_A <=conv_std_logic_vector(j + 1, ADDR_WIDTH);
					end if;					

				else
					if j = Width then
						b <= not b;
						if first_line = "00" then
								first_line <= "01";
						elsif first_line = "01" then
								first_line <= "10";
						elsif first_line = "10" then
								first_line <= "11";
						end if;
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
	variable LeftRL			: pixel;
	variable LineRightLR : Line_array;
begin
	if(PIPELINE_CLOCK'EVENT and PIPELINE_CLOCK = '1') then
		if RESET = '1' then
			LineRight0	<= (others =>(others => '0'));
			LineRight1	<= (others =>(others => '0'));
			LINE_RIGHT	<= (others =>(others => '0'));
			LEFT_RL		<= (others => '0');
			DINL <= (others => '0');
			DINR <= (others => '0');
			i <= "00";
			addrL_a <= (others => '0');
			addrR_a <= (others => '0');
			addrR_b <= (others => '0');
			dinL <= (others => '0');
			dinR <= (others => '0');
			LineRightLR := (others =>(others => '0'));
			LeftRL := (others => '0');
		elsif LINE_VALID_IN = '1' then
			if(i = "01") then
				LineRightLR := LineRightLR(dmax - 2 downto 0) & RIGHT;
				LINE_RIGHT <= LineRightLR;
				LEFT_RL <= LEFT;
			elsif(i = "11") then
				if b = '0' then
						addrL_a <= conv_std_logic_vector(j  , ADDR_WIDTH);
						addrR_a <= conv_std_logic_vector(j , ADDR_WIDTH);
						addrR_b <= conv_std_logic_vector(j  + dmax, ADDR_WIDTH);
						if j  < Width - dmax then
								LineRight0 <= doutR & LineRight0(dmax - 1 downto 1);
						else
								addrR_b <= conv_std_logic_vector(Width - 1, ADDR_WIDTH);
								LineRight0 <= "00000000" & LineRight0(dmax - 1 downto 1);
						end if;
						LineRight1 <= LineRight1(dmax - 2 downto 0) & RIGHT;
						LINE_RIGHT <= LineRight0;
				else
						addrL_a <= conv_std_logic_vector(Width - j - 1, ADDR_WIDTH);
						addrR_a <= conv_std_logic_vector(Width - j - 1, ADDR_WIDTH);
						addrR_b <= conv_std_logic_vector(Width - j - 1 - dmax, ADDR_WIDTH);
						if  j < Width - 1 - dmax then
								LineRight1 <= doutR & LineRight1(dmax - 1 downto 1);
						else
								addrR_b <= conv_std_logic_vector(0, ADDR_WIDTH);
								LineRight1 <= "00000000" & LineRight1(dmax - 1 downto 1);
						end if;                         
						LineRight0 <= LineRight0( dmax - 2 downto 0) & RIGHT  ;
						LINE_RIGHT <= LineRight1;
				end if;
				
				LeftRL :=  doutL;
				dinL <= LEFT;
				dinR <= RIGHT;
				LEFT_RL <= LeftRL;
				
				if first_line /= "11" then
						LEFT_RL <= (others => '0');
						LINE_RIGHT <= (others => (others => '0'));
				end if;
			end if; -- i
			
			i <= i + 1; 
		end if;--line valid / reset
	end if;
end process f;

LOCAL_EX_loop : for k in 0 to dmax-1 generate
begin
	LOCAL_EX : process(PIPELINE_CLOCK) is
		variable LocalCost : LP_element;
	begin
		if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT then
			if RESET = '1' then
				LocalCost := (others => '0');
				LOCAL_COST(k) <= LocalCost;
			elsif LINE_VALID_IN = '1'  then
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
        variable d                              : int_64;
        variable GlobalCost     : GlobalCosts_array;
        begin
                if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
                        if RESET = '1' then
                                dpl0(k) <= 0;
                                GC0(k)  <= (others => '0');
                                GlobalCost := (others => (others => '0'));
                        elsif LINE_VALID_IN = '1' then  
                                if i = "11" then
                                        GlobalCost := GLOBAL_COST_TRE;                                  
                                else
                                        GlobalCost := GLOBAL_COST;
                                end if;
                                d := k * 4;
                                for z in k * 4  to (k + 1) * 4 - 1 loop
                                        if(comparator1(GlobalCost(d), GlobalCost(z))) then
                                                d := z;
                                        end if;
                                end loop;                               
                                dpl0(k) <= d;
                                GC0(k)  <= GlobalCost(d);
                        end if;
                end if; 
        end process DPL00;
end generate PL0;

PL1 : for k in 0 to dmax/16 - 1 generate
begin
        DPL01 : process(PIPELINE_CLOCK) is      
        variable d      : int_64;
        begin
                if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
                        if RESET = '1' then
                                dpl1(k) <= 0;
                                GC1(k)  <= (others => '0');
                        elsif LINE_VALID_IN = '1' then                                  
                                d := k * 4;
                                for z in k * 4 to (k + 1) * 4 - 1 loop
                                        if(comparator1(GC0(d), GC0(z))) then
                                                d := z;
                                        end if;
                                end loop;                               
                                dpl1(k) <= dpl0(d);
                                GC1(k)  <= GC0(d);                                                      
                        end if;
                end if;         
        end process DPL01;
end generate PL1;

GCP : process(PIPELINE_CLOCK) is	

	variable GlobalCostRL	: GlobalCosts_array;
	variable GlobalCostLR	: GlobalCosts_array;
	variable GlobalCostUD 	: GlobalCosts_array;
	variable GlobalCost3	: GlobalCosts_array;
	variable GlobalCost8	: GlobalCosts_array;

begin
	if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
		if RESET = '1' then
			GLOBAL_COST_PREV <= (others => (others => '0'));
			DINLR <= (others => (others => '0'));
			
			GlobalCostRL :=	(others =>(others => '0'));
			GlobalCostUD :=	(others =>(others => '0'));
			GlobalCostLR :=	(others =>(others => '0'));
			GlobalCost3	:=	(others =>(others => '0'));
			GlobalCost8 :=	(others =>(others => '0'));
		elsif LINE_VALID_IN = '1'   then
			if  i = "00" then								
				DINUD_A				<= GLOBAL_COST;
				GlobalCost8			:= GlobalCostUD;
				GLOBAL_COST_PREV	<= GlobalCostRL;
				if first_line = "00" then
					GlobalCostUD := (others =>(others => '0'));             
				else
					GlobalCostUD := DOUTUD_B;
				end if;
			elsif  i = "01" then								
				GlobalCostLR := GLOBAL_COST;
				GlobalCost8 := GlobalCostLR;
			elsif  i = "10" then								
				DINLR <= GlobalCost3;
				GlobalCost3 := DOUTLR;				

				GlobalCostRL := GLOBAL_COST;
				GlobalCost8 := GlobalCostRL;
				
				GLOBAL_COST_PREV <= GlobalCostUD;
			elsif  i = "11" then								
				GLOBAL_COST_PREV <= GlobalCostLR;
				GlobalCost3 := (others =>(others => '0'));
				GlobalCost8 := (others =>(others => '0'));				
			end if;
			
			for k in 0 to dmax - 1 loop
				GlobalCost3(k) := GlobalCost3(k) + GlobalCost8(k)(7 downto 2);
			end loop;
			
			GLOBAL_COST_TRE <= GlobalCost3;		
		end if;
	end if;
end process GCP;



DISP : process(PIPELINE_CLOCK) is	
	variable d				: int_64;
	variable dRL			: int_64;
	variable dLR			: int_64;
	variable dUD			: int_64;


begin
	if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
		if RESET = '1' then
			dd 			<= (others => '0');
			DATA_OUT	<= (others => '0');

			dUD := 0;
			dRL := 0;
			dLR := 0;

			DIND_A	<= conv_std_logic_vector(0, 6);
			
			dinD <= (others => '0');

		elsif LINE_VALID_IN = '1'   then

			
			d := 0;
			for z in 0 to dmax / 16 - 1 loop
					if(comparator1(GC1(d), GC1(z))) then
							d := z;
					end if;                         
			end loop;
			
			if  i = "00" then								
				dRL := dpl1(d);
				dd <= conv_std_logic_vector(dRL, 6);
			elsif  i = "01" then
				if first_line = "00" then
						DIND		<= (others => '0');
						DATA_OUT	<= (others => '0');
				else
						DIND	<= conv_std_logic_vector(dpl1(d), 6);
						DATA_OUT	<= DOUTD(4 downto 0) & "000";
				end if;
			elsif  i = "10" then
				DIND_A	<= conv_std_logic_vector(dpl1(d), 6);
				if first_line = "00" then				
					dUD := 0;
				else
					dUD := conv_integer(doutD_b);
				end if;
			
				dd <= conv_std_logic_vector(dUD, 6);
			elsif  i = "11" then
				dLR := dpl1(d);
				dd <= conv_std_logic_vector(dLR, 6);
			end if;	--i

		end if;	--line valid
	end if;	--reset
end process DISP;
end Behavioral;	
 
--DISP : process(PIPELINE_CLOCK) is	
--	variable d				: int_64;
--	variable dRL			: int_64;
--	variable dLR			: int_64;
--	variable dUD			: int_64;
--
--	variable GlobalCostRL	: GlobalCosts_array;
--	variable GlobalCostLR	: GlobalCosts_array;
--	variable GlobalCostUD 	: GlobalCosts_array;
--	variable GlobalCost3	: GlobalCosts_array;
--	variable GlobalCost8	: GlobalCosts_array;
--
--begin
--	if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
--		if RESET = '1' then
--			GLOBAL_COST_PREV <= (others => (others => '0'));
--			dd 			<= (others => '0');
--			DATA_OUT	<= (others => '0');
--			
--			GlobalCostRL :=	(others =>(others => '0'));
--			GlobalCostUD :=	(others =>(others => '0'));
--			GlobalCostLR :=	(others =>(others => '0'));
--			GlobalCost3	:=	(others =>(others => '0'));
--			GlobalCost8 :=	(others =>(others => '0'));
--
--			dUD := 0;
--			dRL := 0;
--			dLR := 0;
--
--			DINUD_A	<=(others =>(others => '0'));
--			DIND_A	<= conv_std_logic_vector(0, 6);
--			
--			dinD <= (others => '0');
--
--		elsif LINE_VALID_IN = '1'   then
--
--			
--			d := 0;
--			for z in 0 to dmax / 16 - 1 loop
--					if(comparator1(GC1(d), GC1(z))) then
--							d := z;
--					end if;                         
--			end loop;
--			
--			if  i = "00" then								
--				dRL := dpl1(d);
--				dd <= conv_std_logic_vector(dRL, 6);
--			elsif  i = "01" then
--				if first_line = "00" then
--						DIND		<= (others => '0');
--						DATA_OUT	<= (others => '0');
--				else
--						DIND	<= conv_std_logic_vector(dpl1(d), 6);
--						DATA_OUT	<= DOUTD(4 downto 0) & "000";
--				end if;
--			elsif  i = "10" then
--				DIND_A	<= conv_std_logic_vector(dpl1(d), 6);
--				if first_line = "00" then				
--					dUD := 0;
--				else
--					dUD := conv_integer(doutD_b);
--				end if;
--			
--				dd <= conv_std_logic_vector(dUD, 6);
--			elsif  i = "11" then
--				dLR := dpl1(d);
--				dd <= conv_std_logic_vector(dLR, 6);
--			end if;	--i
--
--			if  i = "00" then								
--				DINUD_A				<= GLOBAL_COST;
--				GlobalCost8			:= GlobalCostUD;
--				GLOBAL_COST_PREV	<= GlobalCostRL;
--				if first_line = "00" then
--					GlobalCostUD := (others =>(others => '0'));             
--				else
--					GlobalCostUD := DOUTUD_B;
--				end if;
--			elsif  i = "01" then								
--				GlobalCostLR := GLOBAL_COST;
--				GlobalCost8 := GlobalCostLR;
--			elsif  i = "10" then								
--				DINLR <= GlobalCost3;
--				GlobalCostRL := GLOBAL_COST;
--				GlobalCost3 := DOUTLR;				
--				GlobalCost8 := GlobalCostRL;
--				GLOBAL_COST_PREV <= GlobalCostUD;
--			elsif  i = "11" then								
--				GLOBAL_COST_PREV <= GlobalCostLR;
--				GlobalCost3 := (others =>(others => '0'));
--				GlobalCost8 := (others =>(others => '0'));				
--			end if;
--			
--			for k in 0 to dmax - 1 loop
--				GlobalCost3(k) := GlobalCost3(k) + GlobalCost8(k)(7 downto 2);
--			end loop;
--			
--			GLOBAL_COST_TRE <= GlobalCost3;
--		end if;	--line valid
--	end if;	--reset
--end process DISP;
 
 
