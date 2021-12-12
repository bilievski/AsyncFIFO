library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_unsigned.all;

-- DIN does not require temp signals cause it can drive all FIFO's but FIFO_OUT requires some temp signals


entity data_buff is

	port( ICLK:				 in std_logic;
			OCLK:				 in std_logic;
			reset: 			 in std_logic;
			fill_fifo:		 in std_logic;
			empty_fifo:     in std_logic;
			DIN:        in std_logic_vector(7 downto 0);
			DOUT:       out std_logic_vector(7 downto 0);
			ready_to_fill:  out std_logic;
			fill_done:      out std_logic;
			ready_to_empty: out std_logic;
			start_empty:    out std_logic;
			empty_done:     out std_logic);
end data_buff;

architecture hier of data_buff is

component asynchronFIFO is 
	 Generic ( 		W : natural := 3; -- address Width 
                  D : natural := 8; -- memory depth
                  B : natural := 8  -- bus width
     );
    Port ( resetR : in  STD_LOGIC;
           resetW : in  STD_LOGIC;
           oclk : in  STD_LOGIC;
           iclk : in  STD_LOGIC;
           enR_empty : in  STD_LOGIC;
           enW_fill : in  STD_LOGIC;
           fifo_empty : out  STD_LOGIC;
           fifo_full : out  STD_LOGIC;
           FIFO_OUT : out  STD_LOGIC_VECTOR (B-1 downto 0);
           FIFO_IN : in  STD_LOGIC_VECTOR (B-1 downto 0));
end component asynchronFIFO;


signal isEmpty1, isEmpty2, isEmpty3: std_logic;
signal isFull1, isFull2, isFull3: std_logic; 

signal enWrite1, enWrite2, enWrite3: std_logic;
signal enRead1, enRead2, enRead3: std_logic;

signal raise_ready_to_fill, raise_ready_to_empty: std_logic;
signal temp_DOUT1, temp_DOUT2, temp_DOUT3: std_logic_vector(7 downto 0);

type read_state_type is (RS0, RS1, RS2, RS3);
signal read_state, read_next_state : read_state_type;

type write_state_type is (WS0, WS1, WS2, WS3);
signal write_state, write_next_state : write_state_type;

begin
	FIFO1: asynchronFIFO 
			generic map (3, 8, 8)
			port map (resetR => reset, resetW => reset, oclk => OCLK, iclk => ICLK,
						 enR_empty => enRead1, enW_fill => enWrite1, 
						 fifo_empty => isEmpty1, fifo_full => isFull1, 
						 FIFO_OUT => temp_DOUT1, FIFO_IN => DIN); -- note, DOUT will need to be mapped to 
																		-- auxiliary signal as we cannot map all FIFO outputs to DOUT
						 
	FIFO2: asynchronFIFO 
			generic map (3, 8, 8)
			port map (resetR => reset, resetW => reset, oclk => OCLK, iclk => ICLK,
						 enR_empty => enRead2, enW_fill => enWrite2, 
						 fifo_empty => isEmpty2, fifo_full => isFull2, 
						 FIFO_OUT => temp_DOUT2, FIFO_IN => DIN); -- change DOUT later
						 
	FIFO3: asynchronFIFO 
			generic map (3, 8, 8)
			port map (resetR => reset, resetW => reset, oclk => OCLK, iclk => ICLK,
						 enR_empty => enRead3, enW_fill => enWrite3, 
						 fifo_empty => isEmpty3, fifo_full => isFull3, 
						 FIFO_OUT => temp_DOUT3, FIFO_IN => DIN); -- change DOUT later
						 
						 
						 
	
						 
	ready_to_fill_logic: process(isEmpty1, isEmpty2, isEmpty3, write_state)
		begin

			if (isEmpty1 = '1' or isEmpty2 = '1' or isEmpty3 = '1') and write_state = WS0 then
				raise_ready_to_fill <= '1';
			else
				raise_ready_to_fill <= '0';
			end if;

			
		end process;
		
		
	ready_to_empty_logic: process(isFull1, isFull2, isFull3, read_state)
		begin

			if (isFull1 = '1' or isFull2 = '1' or isFull3 = '1') and read_state = RS0 then
				raise_ready_to_empty <= '1';
			else
				raise_ready_to_empty <= '0';
			end if;
			
		end process;	
		
		
		
	write_side_sync: process (iclk, write_next_state, reset, raise_ready_to_fill)
	begin
		
		if reset = '1' then write_state <= WS0;
		elsif rising_edge(iclk) then write_state <= write_next_state;
			ready_to_fill <= raise_ready_to_fill;
		end if;	
	end process;
	
	write_side_comb: process (write_state, fill_fifo, isFull1, isFull2, isFull3)
	begin
		case write_state is
			when WS0 =>
				enWrite1 <= '0'; enWrite2 <= '0'; enWrite3 <= '0';
				fill_done <= '0';
				if fill_fifo = '1' then
					if isEmpty1 = '1' then 
						write_next_state <= WS1;
					elsif isEmpty2 = '1' then
						write_next_state <= WS2;
					elsif isEmpty3 = '1' then
						write_next_state <= WS3;
					else write_next_state <= WS0;
					end if;
				end if;
			
			when WS1 => 
				if (isFull1 = '0') then
					enWrite1 <= '1';
				else 
					write_next_state <= WS0;
					fill_done <= '1';
				end if;
		
			when WS2 => 
				if (isFull2 = '0') then
					enWrite2 <= '1';
				else 
					write_next_state <= WS0;
					fill_done <= '1';
				end if;
				
				
			when WS3 => 
				if (isFull3 = '0') then
					enWrite3 <= '1';
				else 
					write_next_state <= WS0;
					fill_done <= '1';
				end if;
			
			when others => 
				enWrite1 <= '0'; enWrite2 <= '0'; enWrite3 <= '0';
				write_next_state <= WS0;
				fill_done <= '0';
			
		end case;
				
	end process; 
	
	read_side_sync: process (oclk, read_next_state, reset, raise_ready_to_empty)
	begin
		if reset = '1' then read_state <= RS0;
		elsif rising_edge(oclk) then 
			read_state <= read_next_state;
			ready_to_empty <= raise_ready_to_empty;
		end if;	
	end process;
	
	read_side_comb: process (read_state, empty_fifo, isEmpty1, isEmpty2, isEmpty3)
	begin
		case read_state is
			
			when RS0 =>
				enRead1 <= '0'; enRead2 <= '0'; enRead3 <= '0';
				empty_done <= '0';
				if empty_fifo = '1' then
					if isFull1 = '1' then 
						read_next_state <= RS1;
					elsif isFull2 = '1' then
						read_next_state <= RS2;
					elsif isFull3 = '1' then
						read_next_state <= RS3;
					else read_next_state <= RS0;
					end if;
				end if;
			
			when RS1 => 
				if (isEmpty1 = '0') then
					enRead1 <= '1';
				else 
					read_next_state <= RS0;
					empty_done <= '1';
				end if;
		
			when RS2 => 
				if (isEmpty2 = '0') then
					enRead2 <= '1';
				else 
					read_next_state <= RS0;
					empty_done <= '1';
				end if;
				
				
			when RS3 => 
				if (isEmpty3 = '0') then
					enRead3 <= '1';
				else 
					read_next_state <= RS0;
					empty_done <= '1';
				end if;
				
			when others => 
				enRead1 <= '0'; enRead2 <= '0'; enRead3 <= '0';
				read_next_state <= RS0;
				empty_done <= '0';
			
		end case;
				
	end process; 
	
	--Selector for each of the FIFO outputs tied into the data buffering DOUT
	output_mux: process (oclk, read_state)
	begin
		 if rising_edge(oclk) then 
			
			if read_state = RS1 then -- and enRead2 = '0' and enRead3 = '0' then
				DOUT <= temp_DOUT1;
			elsif read_state = RS2 then -- enRead1 = '0' and enRead2 = '1' and enRead3 = '0' then
				DOUT <= temp_DOUT2;
			elsif read_state = RS3 then -- enRead1 = '0' and enRead2 = '0' and enRead3 = '1' then
				DOUT <= temp_DOUT3;
			else 
				DOUT <= "ZZZZZZZZ";
			end if;
		 end if;
	end process;						 
	
	
end architecture hier;



library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.std_logic_unsigned.all;


-- added last_op_write signal to fix full flag timing


entity asynchronFIFO is
     Generic ( 	W : natural := 3;   
                  D : natural := 8; 
                  B : natural := 8   
     );
    Port ( resetR : in  STD_LOGIC;
           resetW : in  STD_LOGIC;
           oclk : in  STD_LOGIC;
           iclk : in  STD_LOGIC;
           enR_empty : in  STD_LOGIC;
           enW_fill : in  STD_LOGIC;
           fifo_empty : out  STD_LOGIC;
           fifo_full : out  STD_LOGIC;
           FIFO_OUT : out  STD_LOGIC_VECTOR (B-1 downto 0);
           FIFO_IN : in  STD_LOGIC_VECTOR (B-1 downto 0));
end asynchronFIFO;

architecture Behavioral of asynchronFIFO is
    
    signal
			last_op_write,
			full,
			empty
            : std_logic;
    signal 
			w_ptr,
			w_ptr_bin,
			w_ptr_gray,
			w_ptr_gray_sync0,
			w_ptr_gray_sync1,
			sync_w_ptr_bin,
			r_ptr,
			r_ptr_bin,
			r_ptr_gray,
			r_ptr_gray_sync0,
			r_ptr_gray_sync1,
			sync_r_ptr_bin
            : std_logic_vector(W-1 downto 0);
    type
        ramT
            is array (D-1 downto 0) of std_logic_vector(B-1 downto 0);
    signal
        ram
            : ramT;

begin
    write_side : process(iclk)
    begin
        if rising_edge(iclk) then
            if resetW = '1' then
                w_ptr <= (others => '0'); --allows to change all bits despite the size
                w_ptr_gray <= (others => '0');
                sync_r_ptr_bin <= (others => '0');
                r_ptr_gray_sync0 <= (others => '0');
                r_ptr_gray_sync1 <= (others => '0');
            else
                -- incrementation of write pointer
                if enW_fill = '1' and not full = '1' then
                    w_ptr <= w_ptr + '1';
                end if;
                --conversion of binary write pointer to gray code
                w_ptr_gray <= w_ptr xor ('0' & w_ptr(W-1 downto 1));
                --synchronizer
                r_ptr_gray_sync0 <= r_ptr_gray;
                r_ptr_gray_sync1 <= r_ptr_gray_sync0;
                sync_r_ptr_bin <= r_ptr_bin;
            end if;
        end if;
    end process;
	 
	 check_read_write: process(w_ptr, r_ptr) 
	 begin
		if resetW = '1' or resetR = '1' then
			last_op_write <= '0';
		elsif enW_fill = '1' then
			last_op_write <= '1';
		elsif enR_empty = '1' then
			last_op_write <= '0';
		end if;
	 end process;
	 
    --Convert gray-code read pointer to binary 
    r_ptr_bin(W-1) <= r_ptr_gray_sync1(W-1);
    gray2bin_write: for i in W-2 downto 0 generate
        r_ptr_bin(i) <= r_ptr_bin(i+1) xor r_ptr_gray_sync1(i);
    end generate;
	
    --set full flag
    full <= '1' when (w_ptr = sync_r_ptr_bin and last_op_write = '1') else '0';
    fifo_full <= full;
    
	 
    read_side : process(oclk)
    begin
        if rising_edge(oclk) then
            if resetR = '1' then
                r_ptr <= (others => '0');
                r_ptr_gray <= (others => '0');
                sync_w_ptr_bin <= (others => '0');
                w_ptr_gray_sync0 <= (others => '0');
                w_ptr_gray_sync1 <= (others => '0');
            else
                -- incrementation of read pointer
                if enR_empty = '1' and not empty = '1' then
                    r_ptr <= r_ptr + '1';
                end if;
                --conversion of binary read pointer to gray code
                r_ptr_gray <= r_ptr xor ('0' & r_ptr(W-1 downto 1));
                --synchronizer
                w_ptr_gray_sync0 <= w_ptr_gray;
                w_ptr_gray_sync1 <= w_ptr_gray_sync0;
                sync_w_ptr_bin <= w_ptr_bin;
            end if;
        end if;
    end process;
	
    --Convert gray-code write pointer to binary 
    w_ptr_bin(W-1) <= w_ptr_gray_sync1(W-1);
    gray2bin_read: for i in W-2 downto 0 generate
        w_ptr_bin(i) <= w_ptr_bin(i+1) xor w_ptr_gray_sync1(i);
    end generate;
	
    --set empty flag
	empty <= '1' when (r_ptr = sync_w_ptr_bin and last_op_write = '0') else '0';
	fifo_empty <= empty;
    
	--RAM-specific processes
    write_process : process(iclk)
    begin
        if rising_edge(iclk) then
            if enW_fill = '1' and not full = '1' then
                ram(conv_integer(w_ptr)) <= FIFO_IN;
            end if;
      end if;
    end process;
	 
	read_process : process(oclk)
    begin
        if rising_edge(oclk) then
            if enR_empty = '1' and not empty = '1' then
                FIFO_OUT <= ram(conv_integer(r_ptr));
            end if;
      end if;
    end process;
    
end Behavioral;
