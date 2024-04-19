library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity tt_um_sanojn_tlv2556_interface is
    port (
        ui_in   : in  std_logic_vector(7 downto 0);
        uo_out  : out std_logic_vector(7 downto 0);
        uio_in  : in  std_logic_vector(7 downto 0);
        uio_out : out std_logic_vector(7 downto 0);
        uio_oe  : out std_logic_vector(7 downto 0);
        ena     : in  std_logic;
        clk     : in  std_logic;
        rst_n   : in  std_logic
    );
end tt_um_sanojn_tlv2556_interface;

architecture Wrapper of tt_um_sanojn_tlv2556_interface is
  component top is
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
  end component top;
begin
  core: top port map(
    clk      => clk,
    rstn     => rst_n,
    adc_dout => ui_in(0), 
    adc_eoc  => ui_in(1),
    RxD      => ui_in(2),
    CTSn     => ui_in(3),
    
    adc_clk => uo_out(0), 
    adc_cs  => uo_out(1), 
    adc_din => uo_out(2), 
    TxD     => uo_out(3), 
    RTSn    => uo_out(4) 

  );

  uo_out(7 downto 5) <= "000";
  uio_out <= "00000000";
  uio_oe  <= "00000000";

end architecture Wrapper;