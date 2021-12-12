library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;

-- Testbench for data buffering system

entity data_buff_tb is 
end entity;

architecture bh of data_buff_tb is

	component data_buff is
		port(
			ICLK : in std_logic;
			OCLK : in std_logic;
			fill_fifo : in std_logic;
			reset : in std_logic;
			empty_fifo : in std_logic;
			DIN : in std_logic_vector(7 downto 0);
			
			fill_done : out std_logic;
			ready_to_fill : out std_logic;
			ready_to_empty : out std_logic;
			start_empty : out std_logic;
			empty_done : out std_logic;
			DOUT : out std_logic_vector(7 downto 0)
		);
	end component;
	
	signal fill_fifo : std_logic;
	signal DIN : std_logic_vector(7 downto 0);
	signal DOUT : std_logic_vector(7 downto 0);
	signal ICLK : std_logic;
	signal OCLK : std_logic;
	signal reset : std_logic;
	signal empty_fifo : std_logic;
	signal ready_to_fill : std_logic;
	signal fill_done : std_logic;
	signal ready_to_empty : std_logic;
	signal start_empty : std_logic;
	signal empty_done : std_logic;

	constant fast_period : time := 10 ns; -- ICLK frequency is 100 MHz
 	constant slow_period : time := 40 ns; -- OCLK frequency is 25 MHz


begin -- begin architecture description
	
	uut: data_buff
		
		Port map(
			ICLK => ICLK,
			OCLK => OCLK,
			fill_fifo => fill_fifo,
			reset => reset,
			empty_fifo => empty_fifo,
			DIN => DIN,
			
			fill_done => fill_done, 
			ready_to_fill => ready_to_fill,
			ready_to_empty => ready_to_empty, 
			start_empty => start_empty, 
			empty_done => empty_done,  
			DOUT => DOUT	
		);

	

	apply_reset: process
	begin
		reset	<= '0';
		wait for slow_period;
		reset	<= '1';
		wait for fast_period;
		wait for fast_period;
		reset	<= '0';
		wait;
	end process;

	
	
	input_clock: process
	begin
		ICLK	<= '0';
		wait for fast_period;
		ICLK	<= '1';
		wait for fast_period;
	end process;
	
	output_clock: process
	begin
		OCLK	<= '0';
		wait for slow_period;
		OCLK	<= '1';
		wait for slow_period;
	end process;

	
	fill_dbs: process  -- fill data buffering system
	variable val: integer := 0;

	begin

--		for i in 1 to 5 loop 
			wait for slow_period*3;
--		end loop;
		
		-- ****** send 8 bytes to data buffering system *****
--		wait until ready_to_fill = '1';
		wait until ICLK='1' and ICLK'event;
		if (ready_to_fill = '1') then
			fill_fifo <= '1';
			end if;
		wait until ICLK='1' and ICLK'event;
	
		
		for i in 1 to 8 loop
			DIN <= conv_std_logic_vector(val,8);
			val := val+1;
			wait until ICLK='1' and ICLK'event;
		end loop;
		
		fill_fifo <= '0';
		wait until fill_done = '1';
		wait until ICLK='1' and ICLK'event;
		-- ***** sending 8 bytes done ***** 
		
		-- ****** send 8 bytes to data buffering system *****
		wait until ready_to_fill = '1';
		
		
		if (ready_to_fill = '1') then
			fill_fifo <= '1';
			end if;
		-- wait until ICLK='1' and ICLK'event;
		
		for i in 1 to 8 loop
			DIN <= conv_std_logic_vector(val,8);
			val := val+1;
			wait until ICLK='1' and ICLK'event;
		end loop;
		
		fill_fifo <= '0';
--		wait until fill_done = '1';
		wait until ICLK='1' and ICLK'event;
		-- ***** sending 8 bytes done *****


		-- ****** send 8 bytes to data buffering system *****
		wait until ready_to_fill = '1';
		wait until ICLK='1' and ICLK'event;
		fill_fifo <= '1';
		wait until ICLK='1' and ICLK'event;
		
		for i in 1 to 8 loop
			DIN <= conv_std_logic_vector(val,8);
			val := val+1;
			wait until ICLK='1' and ICLK'event;
		end loop;
		
		fill_fifo <= '0';
--		wait until fill_done = '1';
		wait until ICLK='1' and ICLK'event;
		-- ***** sending 8 bytes done *****
		
		for i in 1 to 5 loop
			wait for fast_period;
		end loop;

		wait;
	
	end process;

	empty_dbs: process -- empty data buffering system
	begin
		-- adjust the number of loop iterations to ensure all three FIFOs
		-- are filled by fill_dbs process before we try to empty the FIFOs 
		-- using the VHDL code given below
--		for i in 1 to 50 loop 
			wait for fast_period*80;
			
--		end loop;	
		-- note that DOUT does not need to be “read” explicitly
		-- just display it on the simulation waveforms to verify 
		-- that the emptied values and their sequence is the same
		-- as filled values and their sequence

		-- ****** empty 8 bytes from data buffering system *****
--		wait until ready_to_empty = '1';
--		wait until OCLK='1' and OCLK'event;
--		empty_fifo <= '1';
		wait until OCLK='1' and OCLK'event;
		if (ready_to_empty = '1') then
			empty_fifo <= '1';
			end if;
		wait until OCLK='1' and OCLK'event;
		empty_fifo <= '0';
		wait until empty_done = '1';
		-- ***** emptying 8 bytes done *****
		
		-- ****** empty 8 bytes from data buffering system *****
		wait until ready_to_empty = '1';
--		wait until OCLK='1' and OCLK'event;
--		empty_fifo <= '1';
--		wait until OCLK='1' and OCLK'event;
		if (ready_to_empty = '1') then
			empty_fifo <= '1';
		end if;
		wait until OCLK='1' and OCLK'event;
		empty_fifo <= '0';
		wait until empty_done = '1';
		-- ***** emptying 8 bytes done *****


		-- ****** empty 8 bytes from data buffering system *****
		wait until ready_to_empty = '1';
--		wait until OCLK='1' and OCLK'event;
--		empty_fifo <= '1';
		wait until OCLK='1' and OCLK'event;
		if (ready_to_empty = '1') then
			empty_fifo <= '1';
			end if;
		wait until OCLK='1' and OCLK'event;
		empty_fifo <= '0';
		wait until empty_done = '1';
		-- ***** emptying 8 bytes done *****

		wait;
	end process;

	
end architecture; -- end architecture description
