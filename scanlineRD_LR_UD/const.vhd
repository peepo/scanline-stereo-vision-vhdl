library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

package const is


constant dmax	  	: integer := 16;
constant P1			: integer := 10; 
constant P2			: integer := 40;  
constant scale 	: integer := 10;
constant Width 	: integer := 384;
constant Hight	: integer := 288;
--constant Width 	: integer := 640;
--constant Hight		: integer := 480;

constant ADDR_WIDTH	: integer := 10;  
constant DATA_WIDTH	: integer := 8;

--subtype disparity_element is std_logic_vector(5 downto 0);
subtype LP_element is std_logic_vector(7 downto 0);
subtype pixel is std_logic_vector(7 downto 0);
subtype int_64 is integer range 0 to dmax - 1;
subtype int_640 is integer range 0 to Width - 1;

type    GlobalCosts_array is array(dmax - 1 downto 0) of LP_Element;
type    GlobalCosts_array_disp is array(3 downto 0) of GlobalCosts_array;
type    GlobalCosts_array3 is array(dmax - 1 downto 0) of std_logic_vector(9 downto 0);
type    Line_array is array(dmax - 1 downto 0) of pixel;
type	RL_Line_array is array(1 downto 0, Width - 1 downto 0) of pixel; 
type	line_disparity is array(Width - 1 downto 0) of std_logic_vector(7 downto 0);
type	line_global_cost_array is array(Width - 1 downto 0) of GlobalCosts_array;
type	Address_Width_array is array (dmax - 1 downto 0) of std_logic_vector (ADDR_WIDTH - 1 downto 0);
type  d_disp_array is array(3 downto 0) of int_64;

type    GC_array is array((dmax - 1)/3 downto 0) of LP_Element;


--minimum
type    GlobalCosts_array_32 is array(dmax/2 - 1  downto 0) of LP_element;
type    GlobalCosts_array_16 is array(dmax/4 - 1  downto 0) of LP_element;
type    GlobalCosts_array_8 is array(dmax/8 - 1  downto 0) of LP_element;
type    GlobalCosts_array_4 is array(dmax/16 - 1  downto 0) of LP_element;
type    GlobalCosts_array_2 is array(dmax/32 - 1  downto 0) of LP_element;
type	disp_array_32	is array (dmax/2 - 1 downto 0) of int_64;
type	disp_array_16	is array (dmax/4 - 1 downto 0) of int_64;
type	disp_array_8	is array (dmax/8 - 1 downto 0) of int_64;
type	disp_array_4	is array (dmax/16 - 1 downto 0) of int_64;
type	disp_array_2	is array (dmax/32 - 1 downto 0) of int_64;

 
end const; 