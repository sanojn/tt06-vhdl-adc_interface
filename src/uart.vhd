library ieee;
use ieee.std_logic_1164.all;
package uart is
  component uart_tx is            
    port (  reset:  in std_logic;
            clk:    in std_logic;
            baud:   in std_logic; -- enable @ baud rate
          
            data:   in std_logic_vector(7 downto 0);
            valid:  in std_logic;
            ready:  out std_logic;

            cts_n:  in std_logic; -- Inverted to accomodate CTS# in CH340C chip: Stall when '1'
            TxD:    out std_logic
        );
  end component uart_tx;
  component uart_rx is            
    port (  reset:    in std_logic;
            clk:      in std_logic;
            baudx16:  in std_logic; -- enable @ 16x baud rate
          
            data:   out std_logic_vector(7 downto 0);
            valid:  out std_logic;
            ready:  in  std_logic;

            rts_n:  out std_logic; -- Inverted to accomodate RTS# in CH340C chip: Stall when '1'
            RxD:    in std_logic
        );
  end component uart_rx;
  component fifo is
    generic( depth: natural := 16 );             
    port (  reset:    in std_logic;
            clk:      in std_logic;
     
            in_data:   in  std_logic_vector;
            in_valid:  in  std_logic;
            in_ready:  out std_logic;

            out_data:   out std_logic_vector;
            out_valid:  out std_logic;
            out_ready:  in  std_logic;
       
            full:         out std_logic;
            almost_full:  out std_logic;
            almost_empty: out std_logic;
            empty:        out std_logic
        );
  end component fifo;

end package uart;

library ieee;
use ieee.std_logic_1164.all;
entity uart_tx is            
  port (  reset: in std_logic;
          clk: in std_logic;
          baud: in std_logic; -- enable @ baud rate
          
          data: in std_logic_vector(7 downto 0);
          valid: in std_logic;
          ready: out std_logic;

          cts_n: in std_logic; -- Inverted to accomodate CTS# in CH340C chip: Stall when '1'
          TxD: out std_logic
        );
end entity uart_tx;

library ieee;
use ieee.std_logic_1164.all;
architecture rtl of uart_tx is
  -- sh_reg is an array loaded with stop bit + 8 databits + startbit
  -- and is shifted right towards the output.
  -- When state is all zeroes except for the stop bit,
  -- we're done shifting and can accept a new word
  signal sh_reg : std_logic_vector(9 downto 0):= "0000000001";
begin
  process(clk,reset) is
  begin
    if reset='1' then
      sh_reg <= "0000000001";
      ready <= '0';
    elsif rising_edge(clk) then
      ready <= '0'; -- default assignment
      if baud='1' then
        if sh_reg /= "0000000001" then
          -- shift data towards the output
          sh_reg <= '0' & sh_reg(sh_reg'left downto 1);
        else
          -- load new data when available
          if cts_n = '0' and valid='1' then
            ready <= '1';
            sh_reg  <= '1' & data & '0';
          end if; --cts_n
        end if;
      end if; -- baud
    end if;  -- reset,clk
  end process;
  TxD <= sh_reg(0);
end architecture rtl;


library ieee;
use ieee.std_logic_1164.all;
entity uart_rx is            
  port (  reset:    in std_logic;
          clk:      in std_logic;
          baudx16:  in std_logic; -- enable @ 16x baud rate
          
          data:  out std_logic_vector(7 downto 0);
          valid:  out std_logic := '0';
          ready:  in  std_logic;

          rts_n:  out std_logic := '0'; -- Inverted to accomodate RTS# in CH340C chip: Stall when '1'
          RxD:    in  std_logic
        );
end entity uart_rx;

library ieee;
use ieee.std_logic_1164.all;
architecture rtl of uart_rx is
    signal sh_reg: std_logic_vector(7 downto 0) := (others => '1'); 
begin
  process(clk,reset) is
    variable counter : natural range 0 to 15 := 6;
    type state_t is (idle, start, rdata, stop0, stop1);
   variable state: state_t;
  begin
    if reset = '1' then
      counter := 6;
      sh_reg <= (others => '1');
      state := idle;
      rts_n <= '1';
      valid <= '0';
    elsif rising_edge(clk) then
      valid <= '0'; -- default output
      rts_n <= '0'; -- default output
      case state is
        when idle =>  sh_reg <= (others => '1');
                      counter := 6;
                      rts_n <= '0';
                      if baudx16='1' and RxD='0' then
                        state := start;
                      end if;
        when start => if baudx16='1' then
                        if counter = 0 then
                          if RxD = '0' then -- start bit found
                            sh_reg <= RxD & sh_reg(7 downto 1);
                            state := rdata;
                          else
                            -- This was perhaps only a glitchy input
                            state := idle;
                          end if; -- start bit
                        end if; -- counter
                        counter := (counter - 1) mod 16;
                      end if; -- baudx16
        when rdata => if baudx16='1' then
                        if counter = 0 then
                          sh_reg <= RxD & sh_reg(7 downto 1);
                          if sh_reg(0) = '0' then -- This is the last data bit
                            state := stop0;
                            valid <= '1';
                          end if; -- last data bit
                        end if; -- counter = 0
                        counter := (counter - 1) mod 16;
                      end if; -- baudx16
        when stop0 => -- data is now received and available for reading
                      if ready = '1' then
                        valid <= '0';
                        rts_n <= '0';
                        if RxD = '1' then -- If stop bit arrived, we can go to idle and look for new start bit
                          state := idle;
                        else
                          state := stop1; -- otherwise, wait until the stop bit arrives
                        end if;
                      else
                        valid <= '1'; -- keep flagging available data until it is consumed
                        rts_n <= '1'; -- and try stalling further UART input while we wait
                      end if;                        
        when stop1 => -- data has been consumed
                    -- wait here until the stop bit arrives
                      if RxD = '1' then -- wait for stop bit
                        state := idle;
                      end if;                        
      end case;
    end if; --reset, clk
  end process;
  data <= sh_reg;
end architecture rtl;

library ieee;
use ieee.std_logic_1164.all;
entity fifo is
  generic( depth: natural := 16 );             
  port (  reset:    in std_logic;
          clk:      in std_logic;
          
          in_data:   in  std_logic_vector;
          in_valid:  in  std_logic;
          in_ready:  out std_logic;

          out_data:   out std_logic_vector;
          out_valid:  out std_logic;
          out_ready:  in  std_logic;
       
          full:         out std_logic;
          almost_full:  out std_logic;
          almost_empty: out std_logic;
          empty:        out std_logic
        );
--begin
--  assert depth>0    report "Fifo depth must be non-zero." SEVERITY WARNING;
--  assert depth<=256 report "This FIFO variant is not optimal for depths larger then 256. Use a BlockRAM implementation instead." SEVERITY WARNING;
--  assert depth=2 or depth=4 or depth=8 or depth=16 or depth=32 or depth=64 or depth=128 or depth=256 report "Fifo depth should be a power of 2 for best efficiency" SEVERITY WARNING;
end entity fifo;

architecture rtl of fifo is
  type fifo_t is array (0 to depth-1) of std_logic_vector(in_data'range);
  signal fifo_array : fifo_t := (others => (others => '0'));
  signal rptr : integer range -1 to depth-1;
  signal write_accept : std_logic;
  signal read_accept : std_logic;

  -- Writes shifts data right in the fifo array
  -- Reads are done through a read pointer
  -- The read pointer increments on writes and decrements on reads.
  -- rptr = -1 indicates an empty fifo
begin

  -- input handshake
  write_accept <= '0' when reset='1' else
                  '1' when in_valid='1' and rptr < fifo_array'high else
                  '0';
  in_ready  <= write_accept;

  -- output handshake
  out_valid   <= '0' when reset = '1' else
                 '1' when rptr >= 0 else
                 '0';
  read_accept <= '0' when reset = '1' else
                 '1' when out_ready='1' and rptr >=0 else
                 '0';
  
  process(clk)
  begin
    if rising_edge(clk) then
     if reset='1' then
      rptr <= 0;
      elsif write_accept='1' and read_accept='0' then
        -- shift data and move read pointer with the shift
        fifo_array <= in_data & fifo_array(0 to depth-2);
        rptr <= rptr + 1;
      elsif write_accept='1' and read_accept='1' then
        -- shift data, read pointer stays put
        fifo_array <= in_data & fifo_array(0 to depth-2);
      elsif write_accept='0' and read_accept='1' then
        -- move read pointer back
        rptr <= rptr - 1;
    end if; -- reset
   end if; -- clk
  end process;
  
  out_data <= fifo_array(rptr mod depth);

  empty <= '1' when rptr = -1        else '0';
  full  <= '1' when rptr = fifo_array'high else '0';

  -- if FIFO is big enough, generate "almost" flags at 1/4 and 3/4 
  fifoflags_1: if depth >= 4 generate
    almost_empty <= '1' when rptr <  (    depth / 4 )   else '0';
    almost_full  <= '1' when rptr >= (3 * depth / 4 )-1 else '0';
  end generate;
  -- If FIFO is small, they're the same as full/empty
  fifoflags_2: if depth < 4 generate 
    almost_empty <= '1' when rptr = -1      else '0';
    almost_full  <= '1' when rptr = fifo_array'high else '0';
  end generate;
  
end architecture rtl;
