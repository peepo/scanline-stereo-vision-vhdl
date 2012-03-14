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

	component BRAM_UD is	
	port (
		clk: in std_logic;
		we: in std_logic;
		addr_a: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
		addr_b: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
		din_a: in std_logic_vector (DATA_WIDTH - 1 downto 0 );
		dout_b: out std_logic_vector (DATA_WIDTH -1 downto 0 )
	);
	end component;
	
	component BRAM_Disp is	
	port (
		CLK		: in std_logic;
		WE		: in std_logic;
		ADDR_A	: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
		ADDR_B	: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
		DIN_A	: in std_logic_vector (5 downto 0 );
		DOUT_B	: out std_logic_vector (5 downto 0 )
	);
	end component;

	signal ADDRUD_A	: Address_Width_array;
	signal ADDRUD_B	: Address_Width_array;
	signal DINUD_A	: GlobalCosts_array;
	signal DOUTUD_B	: GlobalCosts_array;

	signal ADDRD_A	: std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal ADDRD_B	: std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal DIND_A	: std_logic_vector (5 downto 0 );
	signal DOUTD_B	: std_logic_vector (5 downto 0 );

	component BRAM_WF is	
	port (
		CLK		: in std_logic;
		WE		: in std_logic;
		ADDR_A	: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
		ADDR_B	: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
		DIN_A	: in std_logic_vector (DATA_WIDTH - 1 downto 0 );
		DOUT_B	: out std_logic_vector (DATA_WIDTH -1 downto 0 )
	);
	end component;
	
	component BRAM_RF is
	port (
		CLK		: in std_logic;
		WE		: in std_logic;
		ADDR_A	: in std_logic_vector (ADDR_WIDTH - 1 downto 0);
		DIN		: in std_logic_vector (DATA_WIDTH - 1 downto 0 );
		DOUT	: out std_logic_vector (DATA_WIDTH -1 downto 0 )
	);
	end component;

	signal ADDRRD_A	: Address_Width_array;
	signal ADDRRD_B	: Address_Width_array;
	signal DINRD_A	: GlobalCosts_array;
	signal DOUTRD_B	: GlobalCosts_array;

	signal ADDRDRD_A	: std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal ADDRDRD_B	: std_logic_vector (ADDR_WIDTH - 1 downto 0);
	signal DINDRD_A		: std_logic_vector (5 downto 0 );
	signal DOUTDRD_B	: std_logic_vector (5 downto 0 );
		
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
	signal DD				: std_logic_vector(5 downto 0);
	signal I				: std_logic_vector(1 downto 0);
	signal J				: int_640;
	signal FIRST_LINE		: std_logic;
	
begin

	BRAM_loop : for k in 0 to dmax-1 generate
	begin
		BramUD : BRAM_UD
		port map(
			CLK		=> PIXEL_CLOCK,
			WE		=> '1',
			ADDR_A	=> ADDRUD_A(k),
			ADDR_B	=> ADDRUD_B(k),
			DIN_A	=> DINUD_A(k),
			DOUT_B	=> DOUTUD_B(k)
		);
		
		BramRD : BRAM_UD
		port map(
			CLK		=> PIXEL_CLOCK,
			WE		=> '1',
			ADDR_A	=> ADDRRD_A(k),
			ADDR_B	=> ADDRRD_B(k),
			DIN_A	=> DINRD_A(k),
			DOUT_B	=> DOUTRD_B(k)
		);
	end generate BRAM_loop;

	BramLineDispUD : BRAM_Disp
	port map(
		CLK		=> PIXEL_CLOCK,
		WE		=> '1',
		ADDR_A	=> ADDRD_A,
		ADDR_B	=> ADDRD_B,
		DIN_A	=> DIND_A,
		DOUT_B	=> DOUTD_B 
	);

	BramLineDispRD : BRAM_Disp
	port map(
		CLK		=> PIXEL_CLOCK,
		WE		=>'1',
		ADDR_A	=> ADDRDRD_A,
		ADDR_B	=> ADDRDRD_B,
		DIN_A	=> DINDRD_A,
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
		else
			if FRAME_VALID_IN = '1' then
				FRAME_VALID_OUT <= '1';
				if LINE_VALID_IN = '1'  then
					LineRightLR := LineRightLR(dmax - 2 downto 0) & RIGHT;
					LINE_RIGHT <= LineRightLR;
					LEFT_RL <= LEFT;
					j <= j + 1;
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

DISP : process(PIPELINE_CLOCK) is	

	variable d	: int_64;
	variable GlobalCostRD: GlobalCosts_array;
	variable dRD 	: int_64;
	variable GlobalCostLR : GlobalCosts_array;
	variable dLR	: int_64;
	variable GlobalCostUD : GlobalCosts_array;
	variable dUD	: int_64;
	variable GlobalCost3 : GlobalCosts_array;
	variable GlobalCost8 : GlobalCosts_array;
begin
	if PIPELINE_CLOCK = '1' and PIPELINE_CLOCK'EVENT  then
		if RESET = '1' or FRAME_VALID_IN = '0' then
			LINE_VALID_OUT <= '0';
			
			GLOBAL_COST_PREV <= (others => (others => '0'));
			dd <= (others => '0');
			GlobalCostRD := (others => (others => '0'));
			dRD := 0;
			GlobalCostUD	:= (others =>(others => '0'));
			dUD := 0;
			GlobalCostLR	:= (others =>(others => '0'));
			dLR := 0;
			GlobalCost3		:= (others =>(others => '0'));
			GlobalCost8		:= (others =>(others => '0'));
			i <= "00";

			ADDRRD_B <=(others =>	conv_std_logic_vector(0,ADDR_WIDTH));
			ADDRRD_A <=(others =>	conv_std_logic_vector(0,ADDR_WIDTH));			
			ADDRDRD_A	<=	conv_std_logic_vector(0, ADDR_WIDTH);					
			ADDRDRD_B	<=	conv_std_logic_vector(0, ADDR_WIDTH);
			DINRD_A 	<= (others =>(others => '0'));
			DINDRD_A	<=	(others => '0');
			
			
			ADDRUD_B <=(others =>  conv_std_logic_vector(0,ADDR_WIDTH));
			ADDRUD_A <=(others => conv_std_logic_vector(0,ADDR_WIDTH));
			DINUD_A <= GLOBAL_COST;
			ADDRD_B	<=conv_std_logic_vector(0, ADDR_WIDTH);
			ADDRD_A	<=conv_std_logic_vector(0, ADDR_WIDTH);
			DIND_A	<= conv_std_logic_vector(0, 6);


		elsif LINE_VALID_IN = '1'    then
			if(i = "10") then
				GlobalCost8 := GlobalCost3;
			else
				GlobalCost8 := GLOBAL_COST;
			end if;
				d := 0;
				for k in 0 to dmax - 1 loop
					if(comparator1(GlobalCost8(d), GlobalCost8(k))) then
						d := k;
					end if;
				end loop;

				if  i = "00" then
					GlobalCostLR := GLOBAL_COST;
					dLR := d;

					GlobalCost8 := GlobalCostLR;
					i<= i + 1;
				elsif  i = "01" then				
					ADDRUD_B <=(others =>  conv_std_logic_vector(j + 1,ADDR_WIDTH));
					ADDRUD_A <=(others => conv_std_logic_vector(j - 1,ADDR_WIDTH));
					DINUD_A <= GLOBAL_COST;
					
					ADDRD_B <=conv_std_logic_vector(j + 1, ADDR_WIDTH);
					ADDRD_A <=conv_std_logic_vector(j - 1, ADDR_WIDTH);
					DIND_A <= conv_std_logic_vector(d, 6);


					if first_line = '1' then
						ADDRUD_A <=(others => conv_std_logic_vector(j + 1,ADDR_WIDTH));
						ADDRD_A <=conv_std_logic_vector(j + 1, ADDR_WIDTH);
						GlobalCostUD := (others =>(others => '0'));		
						dUD := 0;
					else
						dUD := conv_integer(doutD_b);
						GlobalCostUD := DOUTUD_B;		
					end if;
			
								
					GLOBAL_COST_PREV <= GlobalCostRD;
					dd <= conv_std_logic_vector(dRD, 6);

					GlobalCost8 := GlobalCostUD;
					i<= i + 1;
				elsif  i = "10" then
					GLOBAL_COST_PREV <= GlobalCostLR;
					dd <= conv_std_logic_vector(dLR, 6);
						
					LINE_VALID_OUT <= '1';
					DATA_OUT <= conv_std_logic_vector(d* scale, 8);

					GlobalCost3 := (others =>(others => '0'));
					GlobalCost8 := (others =>(others => '0'));
					i<= i + 1;
				elsif  i = "11" then								
									
					ADDRRD_B <=(others =>	conv_std_logic_vector(j + 2,ADDR_WIDTH));
					ADDRRD_A <=(others =>	conv_std_logic_vector(j - 1,ADDR_WIDTH));
					
					ADDRDRD_A	<=	conv_std_logic_vector(j - 1, ADDR_WIDTH);					
					ADDRDRD_B	<=	conv_std_logic_vector(j + 2, ADDR_WIDTH);
					DINRD_A <= GLOBAL_COST;
					DINDRD_A	<=	conv_std_logic_vector(d, 6);
					
					if first_line = '1' then
						ADDRRD_A <=(others =>	conv_std_logic_vector(j + 2,ADDR_WIDTH));
						ADDRDRD_A	<=	conv_std_logic_vector(j + 2, ADDR_WIDTH);
						GlobalCostRD := (others =>(others => '0'));	
						dRD := 0;
					else
						dRD := conv_integer(DOUTDRD_B);
						GlobalCostRD := DOUTRD_B;
					end if;
										
					GLOBAL_COST_PREV <= GlobalCostUD;
					dd <= conv_std_logic_vector(dUD, 6);
					
					GlobalCost8 := GlobalCostRD;
					i<= i + 1;
				end if;	--i

			for k in 0 to dmax - 1 loop
				GlobalCost3(k) := GlobalCost3(k) + GlobalCost8(k)(7 downto 2);
			end loop;
		else
			LINE_VALID_OUT <= '0';			
		end if;	--line valid
	end if;	--reset
end process DISP;

end Behavioral;