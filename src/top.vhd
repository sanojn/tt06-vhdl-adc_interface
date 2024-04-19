
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
entity top is
  port (clk    : in  STD_LOGIC;
        rstn   : in  STD_LOGIC;
        
        adc_clk: out STD_LOGIC;
        adc_cs : out STD_LOGIC;
        adc_din: out STD_LOGIC;
        adc_dout: in STD_LOGIC;
        adc_eoc: in STD_LOGIC;
        
        RxD    : in STD_LOGIC;
        TxD    : out STD_LOGIC;
        RTSn   : out STD_LOGIC;
        CTSn   : in  STD_LOGIC
       );
end top;


library ieee;
use work.uart.all;
use ieee.numeric_std.all;
architecture RTL of top is
 signal Heartbeat: STD_LOGIC;
 signal pace : boolean;
 
 -- UART interfaces, outside the fifo
 signal rx_data, tx_data: std_logic_vector(7 downto 0);
 signal rx_valid, tx_valid: std_logic;
 signal rx_ready, tx_ready: std_logic;
 
 -- UART interfaces, inside the FIFO
 signal uart_rx_data, uart_tx_data: std_logic_vector(7 downto 0);
 signal uart_rx_valid, uart_tx_valid: std_logic;
 signal uart_rx_ready, uart_tx_ready: std_logic;
 signal uart_rx_empty, uart_tx_full: std_logic; -- FIFO flags

 signal baud: std_logic;
 signal baud16: std_logic;

 signal uart_status: std_logic_vector(7 downto 0);
 -- (0) rx_empty
 -- (1) tx_full
 
 signal adc_eoc_d: std_logic;

begin
  
  baud_gen: process(clk) is
    -- BAUDRATE =   230_400, BAUDx16 =  3_686_400 which is 48M / 13.02
    -- variable baud16_counter  : natural range 0 to 12;
    -- BAUDRATE = 1_500_000, BAUDx16 = 24_000_000 which is 48M / 2
    constant prescaler : integer := 13;
    variable baud16_counter : natural range 0 to prescaler - 1;
    variable baud_counter     : natural range 0 to 15;
  begin
    if rising_edge(clk) then
      baud16 <= '0';
      baud <= '0';
      if baud16_counter > 0 then
        baud16_counter := baud16_counter - 1;
      else
        baud16_counter := prescaler -1;
        baud16 <= '1';
      end if;
      if baud16='1' then
        if baud_counter = 0 then
          baud <= '1';
        end if;
        baud_counter := (baud_counter - 1) mod 16;
      end if;
      if rstn='0' then
        baud16_counter := 0;
        baud_counter := 0;
        baud <= '1';
        baud16 <= '1';
      end if;
    end if; -- clk
  end process baud_gen;
  
  rx: uart_rx             
    port map(  reset   => "not"(rstn),
          clk     => clk,
          baudx16 => baud16,
          
          data   => rx_data,
          valid   => rx_valid,
          ready   => rx_ready,

          rts_n   => open, -- connect to rtsn if no fifo is used
          RxD     => RxD
        );
  rx_fifo: fifo
    generic map ( depth => 4 )             
    port map    ( reset => "not"(rstn),
                  clk   => clk,
          
                  in_data  => rx_data,
                  in_valid => rx_valid,
                  in_ready => rx_ready,

                  out_data  => uart_rx_data,
                  out_valid => uart_rx_valid,
                  out_ready => uart_rx_ready,
            
            full         => open,
            almost_full  => RTSn, -- stall sender if fifo is close to full
            almost_empty => open,
            empty        => uart_rx_empty
        );
  
  tx: uart_tx           
    port map ( reset => "not"(rstn),
          clk   => clk,
          baud  => baud,
          
          data  => tx_data,
          valid => tx_valid,
          ready => tx_ready,

          cts_n => ctsn,
          TxD => TxD
        );
  tx_fifo: fifo
    generic map ( depth => 4 )             
    port map    ( reset => "not"(rstn),
                  clk   => clk,
          
                  in_data  => uart_tx_data,
                  in_valid => uart_tx_valid,
                  in_ready => uart_tx_ready,

                  out_data  => tx_data,
                  out_valid => tx_valid,
                  out_ready => tx_ready,
            
            full         => uart_tx_full,
            almost_full  => open, 
            almost_empty => open,
            empty        => open
        );

--  process(clk) is
--    variable counter: integer range 0 to 31 := 0;
--  begin
--    if baud='1' then
--      counter := (counter + 1) mod 32;
--      if counter = 31 then
--        reset <= '0';
--      end if;
--    end if;
--  end process;

  tlv2556: process (clk,rstn) is
    function ascii2hex(ascii: std_logic_vector) return std_logic_vector is
    begin
      if unsigned(ascii(7 downto 0)) < x"40" then
        -- it's a digit, return the LS nybble
        return ascii(3 downto 0);
      else
        -- it's A-F which is 41-46, return LS nybble plus 9 to convert to 0xa - 0xf
        return std_logic_vector(unsigned(ascii(3 downto 0)) + 9);
      end if;
    end function;
    
    variable counter: integer range 0 to 4; -- prescaler for 10MHz ADC clock
    type state_t is ( init, idle, command, delay, convert, readout, reply_single, reply_single2, reply_cont, pause );
    type mode_t is (single, continuous);
    variable state : state_t;
    variable outreg: std_logic_vector(11 downto 0);
    variable inreg: std_logic_vector(11 downto 0);
    variable mode: mode_t;
    variable channel: unsigned(3 downto 0);
    variable line_pos: integer range 0 to 10;
    variable calculating : boolean;
    variable mod10 : unsigned(3 downto 0);
    variable leadingspaces : boolean;
  begin
    if rstn='0' then
      counter := 0;
      adc_cs  <= '1';
      adc_din <= '0';
      adc_clk <= '0';
      state   := init;
      mode    := single;
      channel := "0000";
      line_pos := 0;
      outreg  := "111101000001"; -- ADC init word: write CFGR2, 2.048 reference, EOC, normal mode, plus trailer and marker in the rightmost bit
      
      uart_rx_ready <= '0';
      uart_tx_valid <= '0';
      uart_tx_data  <= "00000000";
      calculating := false;
      mod10 := "0000";
      leadingspaces := true;
    elsif rising_edge(clk) then
      uart_rx_ready <= '0';
      uart_tx_valid <= '0';
      adc_clk <= '0';
      adc_cs   <= '1';
      case state is
        when init =>
                    adc_cs   <= '0';
                    if counter < 3 then
                      adc_clk <= '0';
                    else
                      adc_clk <= '1';
                    end if;
                    if counter = 4 then
                      if outreg = "100000000000" then
                        state := idle;
                      end if;
                      outreg := outreg(outreg'left-1 downto 0) & '0';
                    end if;
                    counter := (counter + 1) mod 5;

        when idle =>
                    if adc_eoc /= '0' then -- ADC is available for new operation
                      uart_rx_ready <= '1';
                      if uart_rx_ready='1' and uart_rx_valid='1' then
                        uart_rx_ready <= '0';
                        if uart_rx_data =  "00001101" or uart_rx_data = "00001010" then -- CR or NL
                          -- enter continuous mode
                          state := command;
                          mode := continuous;
                          channel := "0000";
                          line_pos := 0;
                          outreg := std_logic_vector(channel) & "00000001"; -- Channel 0, 12 bits, MSB first, unipolar, plus trailer and end marker
                        else
                          state  := command;
                          mode := single;
                          outreg := ascii2hex(uart_rx_data) & "00000001"; -- 12 bits, MSB first, unipolar, plus trailer and end marker
                          if outreg(11 downto 9) = "111" then
                            -- invalid channel request. Skip coonversion and return 0.
                            inreg := (others => '0');
                            state := reply_single;
                          end if;
                        end if;
                      elsif mode=continuous then
                        state := command;
                        outreg := std_logic_vector(channel) & "00000001"; -- 12 bits, MSB first, unipolar, plus trailer and end marker
                      end if;
                    end if;
                          
        when command =>
                    adc_cs   <= '0';
                    if counter < 3 then
                      adc_clk <= '0';
                    else
                      adc_clk <= '1';
                    end if;
                    if counter = 4 then
                      if outreg = "100000000000" then
                        outreg := "000000000001"; -- bogus conversion instruction for readout sequence
                        state := delay;
                      else
                        outreg := outreg(outreg'left-1 downto 0) & '0';
                      end if;
                    end if;
                    counter := (counter + 1) mod 5;

        when delay =>  -- wait until conversion starts
                    if adc_eoc = '0' then
                      state := convert;
                    end if;
 
        when convert =>
                    if adc_eoc = '1' then
                        state := readout;
                    end if;
        
        when readout =>
                    adc_cs   <= '0';
                    if counter < 3 then
                      adc_clk <= '0';
                    else
                      adc_clk <= '1';
                    end if;
                    if counter = 4 then
                      inreg  := inreg(inreg'left-1 downto 0) & adc_dout;
                      if outreg = "100000000000" then
                        if mode = single then
                          state := reply_single;
                        else
                          state := reply_cont;
                          inreg := '0' & inreg(11 downto 1); -- shift to get result in mV
                        end if;
                      end if;
                      outreg := outreg(outreg'left-1 downto 0) & '0';
                    end if;
                    counter := (counter + 1) mod 5;

        -- Single conversion reply
        when reply_single =>
                    uart_tx_data  <= "01" & inreg(11 downto 6);
                    uart_tx_valid <= '1';
                    if uart_tx_valid='1' and uart_tx_ready='1' then
                      uart_tx_valid <= '0';
                      state := reply_single2;
                    end if;
                    
        when reply_single2 =>
                    uart_tx_data  <= "01" & inreg(5 downto 0);
                    uart_tx_valid <= '1';
                    if uart_tx_valid='1' and uart_tx_ready='1' then
                      state := idle;
                      uart_tx_valid <= '0';
                    end if;

        -- Continuous reply
        when reply_cont =>
                    uart_tx_valid <= '1';
                    case line_pos is
                      when 0  => uart_tx_data  <= "00001100";                         -- FF;
                      when 1  => if channel = 10 then                                 -- Channel Number
                                   uart_tx_data  <= "00110001";
                                 else
                                   uart_tx_data  <= "00100000";
                                 end if;
                      when 2  => if channel = 10 then                                 -- Channel Number
                                   uart_tx_data  <= "00110000";
                                 else
                                   uart_tx_data  <= "0011" & std_logic_vector(channel);
                                 end if;
                      when 3  => uart_tx_data  <= "00111010";                         -- Colon
                      when 4  => uart_tx_data  <= "00100000";                         -- Space
                      when 5  => uart_tx_valid <= '0';                                -- thousands digit
                                 if unsigned(inreg) >= "1111101000" then
                                   calculating := true;
                                   inreg := std_logic_vector(unsigned(inreg) - "1111101000");
                                   mod10 := mod10 + 1;
                                 else
                                   uart_tx_valid <= '1';
                                   calculating := false;
                                   if mod10 = "0000" then 
                                     uart_tx_data <= "00100000";
                                     leadingspaces := true;
                                   else
                                     uart_tx_data <= "0011" & std_logic_vector(mod10);
                                     leadingspaces := false;
                                   end if;
                                 end if;
                      when 6  => uart_tx_valid <= '0';                                 -- hundreds digit
                                 if unsigned(inreg) >= "01100100" then 
                                   calculating := true;
                                   inreg := std_logic_vector(unsigned(inreg) - "01100100");
                                   mod10 := mod10 + 1;
                                 else
                                   uart_tx_valid <= '1';
                                   calculating := false;
                                   if mod10 = "0000" then
                                     if leadingspaces then 
                                       uart_tx_data <= "00100000";
                                     else
                                       uart_tx_data <= "00110000";
                                       leadingspaces := false;
                                     end if;
                                   else
                                     uart_tx_data <= "0011" & std_logic_vector(mod10);
                                     leadingspaces := false;
                                   end if;
                                 end if;
                      when 7  => uart_tx_valid <= '0';                                 -- tens digit
                                 if unsigned(inreg) >= "00001010" then 
                                   calculating := true;
                                   inreg := std_logic_vector(unsigned(inreg) - "00001010");
                                   mod10 := mod10 + 1;
                                 else
                                   uart_tx_valid <= '1';
                                   calculating := false;
                                   if mod10 = "0000" then
                                     if leadingspaces then 
                                       uart_tx_data <= "00100000";
                                     else
                                       uart_tx_data <= "00110000";
                                       leadingspaces := false;
                                     end if;
                                   else
                                     uart_tx_data <= "0011" & std_logic_vector(mod10);
                                     leadingspaces := false;
                                   end if;
                                 end if;
                      when 8  => uart_tx_data <= "0011" & std_logic_vector(inreg(3 downto 0)); -- ones digit
                                 leadingspaces := false;
                                 mod10 := "0000";
                      when 9  => uart_tx_data  <= "00001101";                         -- CR
                      when 10 => uart_tx_data  <= "00001010";                         -- NL
                    end case;
                    if not calculating and uart_tx_valid='1' and uart_tx_ready='1' then
                      uart_tx_valid <= '0';
                      mod10 := "0000";
                      if line_pos=10 and channel /= 10 then
                        outreg := std_logic_vector(channel) & "00000001"; -- 12 bits, MSB first, unipolar, plus trailer and end marker
                        line_pos := 1;
                        state := command;
                        channel := channel + 1;
                      elsif line_pos > 9 then
                        line_pos := 0;
                        channel := "0000";
                        state := pause;
                      else
                        line_pos := line_pos + 1;
                      end if;
                    end if;
                    
        when pause =>
                    if uart_rx_valid ='1' or pace then
                      state := idle;
                    end if;
                     
      end case;     
      adc_din <= outreg(outreg'left);
    end if;      
  end process tlv2556;

  
  process(clk) is
    variable prescale1: integer range 0 to 15 := 0;
    subtype prescaleRange is integer range 0 to 2_999_999;
    variable prescaler: prescaleRange := 0;
  begin
    if rising_edge(clk) then
     pace <= false;
     prescale1 := (prescale1 +1) mod 16;
     if prescale1=0 then
      if prescaler mod 2**18 = 0 then
        pace <= true;
      end if;
      if prescaler=0 then
        prescaler:=prescaleRange'high;
      else
        prescaler := prescaler-1;
       end if;
       if prescaler >= 2**21 then
        Heartbeat <= '1';
      else
        Heartbeat <= '0';
      end if;
    end if;
   end if;
  end process;

end RTL;

